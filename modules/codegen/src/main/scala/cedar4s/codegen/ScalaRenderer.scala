package cedar4s.codegen

import cedar4s.schema.{ExtensionType, OwnershipType, PrimitiveType, QualifiedName, SchemaType}

/** Renders Cedar IR to Scala source code.
  *
  * Generates:
  *   - EntityIds object with newtypes for all entity IDs
  *   - PolicyDomain enum
  *   - Actions object with typed action case objects
  *   - Resource case class with phantom types and factory methods
  *   - Ownership marker traits
  */
object ScalaRenderer {
  private def escapeIdent(name: String): String = ScalaKeywords.escape(name)

  /** Result of rendering - a map of filename -> content.
    */
  type RenderResult = Map[String, String]

  /** Render all Scala files from the IR.
    *
    * @param ir
    *   The Cedar IR to render
    * @param pkg
    *   Target package name
    * @return
    *   Map of filename -> Scala source code
    */
  def render(ir: CedarIR, pkg: String): RenderResult = {
    // Extract namespace name from IR for the DSL entrypoint
    val dslName = ir.namespace.split("::").last

    Map(
      "EntityIds.scala" -> renderEntityIds(ir, pkg),
      "PolicyDomain.scala" -> renderPolicyDomain(ir, pkg),
      "CommonTypes.scala" -> renderCommonTypes(ir, pkg),
      "EntityFetchers.scala" -> renderEntityFetchers(ir, pkg, dslName),
      "EntitySchema.scala" -> renderEntitySchema(ir, pkg),
      s"$dslName.scala" -> renderDsl(ir, pkg, dslName),
      "PrincipalEvidence.scala" -> renderPrincipalEvidence(ir, pkg, dslName),
      "Contexts.scala" -> renderContexts(ir, pkg),
      "HasParentEvidence.scala" -> renderHasParentEvidence(ir, pkg)
    )
  }

  // ============================================================================
  // EntityIds - Newtypes for all entity IDs
  // ============================================================================

  /** Generate newtypes for all entity IDs.
    *
    * Each entity gets a corresponding ID type: User -> UserId, Document -> DocumentId, etc. These newtypes provide
    * type-safe ID handling with zero runtime overhead.
    */
  private def renderEntityIds(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))

    val imports = s"""import cedar4s.{Bijection, Newtype}"""

    // Generate newtype objects for each entity
    val newtypes = entities
      .map { entity =>
        val idTypeName = s"${entity.name}Id"
        s"""/** ID type for ${entity.name} entities */
object $idTypeName extends Newtype[String]
type $idTypeName = $idTypeName.Type"""
      }
      .mkString("\n\n")

    s"""$header
$imports

/**
 * Entity ID newtypes for type-safe ID handling.
 * 
 * Each entity type has a corresponding ID newtype that wraps String
 * with zero runtime overhead. This prevents accidentally mixing up
 * IDs from different entity types.
 * 
 * == Usage ==
 * 
 * {{{
 * import $pkg.EntityIds._
 * 
 * // Create typed IDs
 * val userId: UserId = UserId("user-123")
 * val docId: DocumentId = DocumentId("doc-456")
 * 
 * // Extract underlying string
 * val str: String = userId.value
 * 
 * // Use with Resource factories
 * Resource.document(folderId, docId)
 * 
 * // Use the bijection for conversions
 * val bij: Bijection[String, UserId] = UserId.bijection
 * }}}
 */
object EntityIds {
$newtypes
}

// Re-export ID types at package level for convenience
${entities.map(e => s"type ${e.name}Id = EntityIds.${e.name}Id.Type").mkString("\n")}
"""
  }

  // ============================================================================
  // PolicyDomain Enum
  // ============================================================================

  private def renderPolicyDomain(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))

    val imports = s"""import cedar4s.schema.{OwnershipType, ShapeId, ShapeTag, Hints, CedarHints}
import cedar4s.auth.{PolicyDomain => PolicyDomainMarker}"""

    // Generate case objects for ALL entities (not just those with actions)
    val enumCases = entities
      .map { e =>
        val ownership = ownershipTypeName(e.ownership)
        val scalaName = escapeIdent(e.name)
        s"""  case object $scalaName extends PolicyDomain {
    val entityType: String = "${e.name}"
    val ownership: OwnershipType = OwnershipType.$ownership
  }"""
      }
      .mkString("\n\n")

    val allValues = entities.map(e => escapeIdent(e.name)).mkString(", ")

    val companions = entities
      .map { e =>
        val scalaName = escapeIdent(e.name)
        s"""
object ${e.name}Domain extends ShapeTag.Companion[PolicyDomain.${scalaName}.type] {
  val id: ShapeId = ShapeId("${ir.namespace}", "${e.name}")
  val hints: Hints = Hints(
    CedarHints.ownership(OwnershipType.${ownershipTypeName(e.ownership)})${if (e.parentChain.nonEmpty)
            s""",
    CedarHints.parents(List(${e.parentChain.map(p => s""""$p"""").mkString(", ")}))"""
          else ""}
  )
}"""
      }
      .mkString("\n")

    s"""$header
$imports

/**
 * Policy domains derived from Cedar schema.
 * 
 * Each domain corresponds to an entity type that has actions defined.
 * Extends [[cedar4s.auth.PolicyDomain]] to enable type-safe parent access.
 */
sealed trait PolicyDomain extends PolicyDomainMarker {
  def entityType: String
  def ownership: OwnershipType
  /** Backward-compatible alias for entityType */
  def value: String = entityType
}

object PolicyDomain {
$enumCases

  val values: List[PolicyDomain] = List($allValues)
}
$companions
"""
  }

  // ============================================================================
  // Actions Object
  // ============================================================================

  private def renderActions(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    // Use buildDomainsByActionDomain for Actions - group by action name prefix
    val domains = SchemaToIR.buildDomainsByActionDomain(ir)

    val imports = s"""import cedar4s.auth.CedarAction
import cedar4s.schema.{ShapeId, ShapeTag, Hints, CedarHints}"""

    val domainObjects = domains
      .map { d =>
        renderDomainActions(d, ir.namespace)
      }
      .mkString("\n")

    val allActions = domains
      .flatMap(_.actions)
      .map { a =>
        s"${a.domain}.${a.objectName}"
      }
      .mkString(", ")

    s"""$header
$imports

object Actions {

  /**
   * Base trait for all Cedar actions.
   * 
   * Actions have phantom type members to enable compile-time checking
   * that action/resource pairs are compatible:
   * - Domain: The policy domain (e.g., PolicyDomain.Mission.type)
   * - Ownership: The ownership classification (RootOwnership, DirectOwnership, IndirectOwnership)
   * 
   * Extends [[CedarAction]] from cedar4s-core for compatibility with the auth DSL.
   */
  sealed trait Action extends CedarAction {
    /** Phantom type for the policy domain */
    type Domain <: PolicyDomain
    
    /** Phantom type for ownership classification */
    type Ownership
    
    /** Backward-compatible alias for name */
    def value: String = name
    
    /** Domain this action belongs to */
    def domain: PolicyDomain
  }
$domainObjects

  /** All actions across all domains */
  val all: Set[Action] = Set($allActions)
  
  /** Find action by Cedar action string */
  def fromCedarAction(cedarAction: String): Option[Action] =
    all.find(_.cedarAction == cedarAction)
  
  /** Find action by domain and simple name */
  def fromString(domain: PolicyDomain, name: String): Option[Action] =
    all.find(a => a.domain == domain && a.name == name)
  
  /**
   * Get all action names for a specific domain.
   * 
   * Useful for capability checks where you need to test all possible
   * actions for a domain.
   * 
   * @param domain The policy domain to get actions for
   * @return Set of action names (e.g., Set("create", "read", "update"))
   */
  def allForDomain(domain: PolicyDomain): Set[String] = domain match {
${domains
        .map(d => s"""    case PolicyDomain.${escapeIdent(d.name)} => ${escapeIdent(d.name)}.all.map(_.name)""")
        .mkString("\n")}
  }
}
"""
  }

  private def renderDomainActions(domain: DomainIR, namespace: String): String = {
    // Use Resource. prefix since Actions are nested in the same DSL object as Resource
    val ownershipType = domain.entity.ownership match {
      case OwnershipType.Root     => "Resource.RootOwnership"
      case OwnershipType.Direct   => "Resource.DirectOwnership"
      case OwnershipType.Indirect => "Resource.IndirectOwnership"
    }

    val actions = domain.actions
      .map { a =>
        s"""    case object ${a.objectName} extends ${domain.name}Action {
      val name = "${a.name}"
      val isCollectionAction = ${a.isCollectionAction}
    }"""
      }
      .mkString("\n\n")

    val actionNames = domain.actions.map(a => a.objectName).mkString(", ")

    s"""
  object ${escapeIdent(domain.name)} {
    sealed trait ${domain.name}Action extends Action {
      type Domain = PolicyDomain.${escapeIdent(domain.name)}.type
      type Ownership = $ownershipType

      val domain: PolicyDomain = PolicyDomain.${escapeIdent(domain.name)}
      def cedarAction: String = s\"\"\"$namespace::Action::"${domain.name}::$$name"\"\"\"
    }

$actions

    val all: Set[${domain.name}Action] = Set($actionNames)
  }"""
  }

  // ============================================================================
  // Resource Case Class with Phantom Types
  // ============================================================================

  private def renderResource(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))
    val namespace = ir.namespace
    val pkgLastSegment = pkg.split("\\.").last

    // All entity IDs are newtypes, no additional imports needed
    val imports = s"""import cedar4s.auth.CedarResource
import cedar4s.entities.ResourceRef
import cedar4s.schema.CedarEntityUid"""

    // Generate ownership marker traits
    val ownershipTraits = s"""
/**
 * Phantom type markers for resource ownership classification.
 * These are sealed traits used only at the type level to enforce
 * that actions are called with correctly-shaped resources.
 */
sealed trait RootOwnership
sealed trait DirectOwnership
sealed trait IndirectOwnership

// Backward-compatible type aliases
type TenantOwnership = RootOwnership
type LocationOwnership = IndirectOwnership

"""

    // Generate the Resource case class
    val resourceClass = s"""
/**
 * Type-safe resource wrapper for Cedar authorization.
 * 
 * This wrapper contains all the information needed to:
 * 1. Create Cedar entity UIDs for authorization requests
 * 2. Build entity hierarchies for parent chain traversal
 * 
 * Extends [[CedarResource]] from cedar4s-core for compatibility with the auth DSL.
 * 
 * @tparam D The domain phantom type (e.g., PolicyDomain.Mission.type)
 * @tparam O The ownership phantom type marker (e.g., DirectOwnership)
 */
final case class Resource[D <: PolicyDomain, O] private[$pkgLastSegment] (
    entityType: String,
    entityId: Option[String],
    parents: List[(String, String)]
) extends CedarResource {
  
  /** Backward-compatible alias for entityId */
  def resourceId: Option[String] = entityId
  
  /** Format as Cedar entity UID */
  def toCedarEntityUid: CedarEntityUid = {
    // entityType already includes namespace (e.g., "DocShare::Document")
    entityId match {
      case Some(id) => CedarEntityUid(entityType, id)
      case None =>
        // For collection contexts, use the immediate parent
        parents.lastOption match {
          case Some((parentType, parentId)) => CedarEntityUid(parentType, parentId)
          case None => CedarEntityUid(entityType, "unknown")
        }
    }
  }
  
  /** Format as Cedar entity UID string */
  def toCedarEntity: String = entityId match {
    case Some(id) => s\"\"\"$$entityType::"$$id"\"\"\"
    case None => 
      // For collection contexts, use the immediate parent
      parents.lastOption match {
        case Some((parentType, parentId)) => s\"\"\"$$parentType::"$$parentId"\"\"\"
        case None => entityType
      }
  }
  
  /** Get parent entity UIDs for Cedar context */
  def parentEntities: List[String] =
    parents.map { case (t, id) => s\"\"\"$$t::"$$id"\"\"\" }
  
  /** Get the customer ID from parents (if present) */
  def customerId: Option[String] = entityId.filter(_ => entityType == "$namespace::Customer")
    .orElse(parents.collectFirst { case ("$namespace::Customer", id) => id })
  
  /** Get the location ID from parents (if present) */
  def locationId: Option[String] = entityId.filter(_ => entityType == "$namespace::Location")
    .orElse(parents.collectFirst { case ("$namespace::Location", id) => id })
  
  /** Get the domain as PolicyDomain */
  def domain: PolicyDomain = PolicyDomain.values.find(d => entityType.endsWith("::" + d.entityType) || entityType == d.entityType).get
}"""

    s"""$header
$imports
$ownershipTraits
$resourceClass

object Resource {

  /**
    * Low-level constructor for dynamic resource creation.
    * 
    * Use this when you need to construct resources dynamically at runtime
    * without knowing the concrete types at compile time. 
    * 
    * For normal authorization checks, use the DSL's `.on(id)` pattern which
    * automatically resolves the resource hierarchy via EntityStore.
    */
  def apply[D <: PolicyDomain, O](
      entityType: String,
      entityId: Option[String],
      parents: List[(String, String)]
  ): Resource[D, O] = new Resource(entityType, entityId, parents)

  /**
    * Construct a Resource from a ResourceRef.
    * 
    * Used by the deferred `.on(id)` pattern when resolving resources
    * via EntityStore.
    */
  def fromResourceRef[D <: PolicyDomain, O](ref: ResourceRef): Resource[D, O] = {
    // ResourceRef already has namespace-prefixed entity types
    new Resource(ref.entityType, ref.entityId, ref.parents)
  }
}
"""
  }

  /** Build factory method parameters using generated newtypes.
    *
    * All entity IDs are now newtypes (e.g., CustomerId, MissionId).
    *
    * @return
    *   List of (paramName, newtypeType) tuples
    */
  private def buildNewtypeFactoryParams(
      entity: EntityIR,
      ir: CedarIR
  ): List[(String, String)] = {
    // Parent IDs in order - use the parent entity's ID type
    val parentParams = entity.parentChain.map { parentName =>
      val paramType = s"${parentName}Id" // Generated newtype name
      val paramName = parentName.take(1).toLowerCase + parentName.drop(1) + "Id"
      (paramName, paramType)
    }

    // This entity's ID
    val entityParamType = s"${entity.name}Id" // Generated newtype name
    val entityParamName = "id"

    parentParams :+ (entityParamName, entityParamType)
  }

  // ============================================================================
  // Helpers
  // ============================================================================

  private def fileHeader(pkg: String): String =
    s"""// AUTO-GENERATED - DO NOT EDIT
package $pkg
"""

  private def ownershipTypeName(ownership: OwnershipType): String = ownership match {
    case OwnershipType.Root     => "Root"
    case OwnershipType.Direct   => "Direct"
    case OwnershipType.Indirect => "Indirect"
  }

  private def ownershipPhantomType(ownership: OwnershipType, dslName: String = ""): String = {
    val base = ownership match {
      case OwnershipType.Root     => "RootOwnership"
      case OwnershipType.Direct   => "DirectOwnership"
      case OwnershipType.Indirect => "IndirectOwnership"
    }
    if (dslName.nonEmpty) s"$dslName.Resource.$base" else base
  }

  // ============================================================================
  // Entity Classes with CedarEntityType instances
  // ============================================================================

  private def renderEntities(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))
    val namespace = ir.namespace

    // Import the generated ID types
    val imports = s"""import cedar4s.entities.{CedarEntity, CedarEntityType, CedarValue}
import cedar4s.schema.CedarEntityUid"""

    val entityClasses = entities
      .map { entity =>
        renderEntityClass(entity, ir, namespace)
      }
      .mkString("\n\n")

    s"""$header
$imports

/**
 * Entity classes for Cedar authorization.
 * 
 * Each case class has a companion object with a [[CedarEntityType]] instance
 * that enables automatic conversion to Cedar entities and type-safe registration
 * with [[cedar4s.entities.EntityStore]].
 * 
 * Entity classes are namespaced under `Entities` to avoid collision with `Actions`:
 * 
 * {{{
 * import $pkg.Entities._
 * import $pkg.Actions._
 * 
 * // Entities.Document vs Actions.Document are now distinct
 * }}}
 * 
 * == Usage ==
 * {{{
 * // Implement an EntityFetcher that returns the generated entity class
 * class DocumentFetcherImpl(db: Database)(implicit ec: ExecutionContext)
 *     extends EntityFetcher[Future, Entities.Document] {
 *   def fetch(id: String): Future[Option[Entities.Document]] = 
 *     db.run(docDao.get(id)).map(_.map(toDocument))
 * }
 * 
 * // Register with EntityStore (CedarEntityType resolved from companion)
 * val store = EntityStore.builder[Future]()
 *   .register[Entities.Document](new DocumentFetcherImpl(db))
 *   .build()
 * 
 * // Direct conversion via extension methods
 * import cedar4s.entities.CedarEntityType._
 * val cedarEntity: CedarEntity = document.toCedarEntity
 * }}}
 */
object Entities {
$entityClasses
}
"""
  }

  // ============================================================================
  // Common Types
  // ============================================================================

  private def renderCommonTypes(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    if (ir.commonTypes.isEmpty) {
      return s"""$header
// No common types defined in schema.
"""
    }

    val imports = s"""import cedar4s.entities.CedarValue"""
    val definitions = ir.commonTypes
      .map { common =>
        renderCommonType(common, ir.namespace)
      }
      .mkString("\n\n")

    s"""$header
$imports

$definitions
"""
  }

  private def renderCommonType(common: CommonTypeIR, namespace: String): String = {
    val scalaName = escapeIdent(common.name)
    val doc = common.doc.map(d => s"/** $d */\n").getOrElse("")

    common.typeExpr match {
      case SchemaType.Record(_) =>
        renderCommonRecordType(scalaName, common.attributes, namespace, doc)
      case SchemaType.SetOf(SchemaType.Record(_)) =>
        val element = common.elementRecord.get
        val elementClass = renderCommonRecordType(element.className, element.attributes, namespace, "")
        val aliasType = s"type $scalaName = Set[${element.className}]"
        s"""$elementClass
$doc$aliasType"""
      case other =>
        val scalaType = commonTypeToScalaType(other)
        s"${doc}type $scalaName = $scalaType"
    }
  }

  private def renderCommonRecordType(
      className: String,
      attributes: List[AttributeIR],
      namespace: String,
      doc: String
  ): String = {
    val fields = attributes
      .map { attr =>
        val scalaType = attr.nestedRecord match {
          case Some(nested) =>
            val nestedType = s"$className.${nested.className}"
            if (attr.optional) s"Option[$nestedType]" else nestedType
          case None => attr.scalaType
        }
        s"  ${attr.fieldName}: $scalaType"
      }
      .mkString(",\n")

    val toCedarFields = attributes
      .map { attr =>
        val valueExpr = renderCedarValueExpr(attr, attr.fieldName, namespace)
        val quote = '"'
        s"    $quote${attr.name}$quote -> $valueExpr"
      }
      .mkString(",\n")

    val nestedClasses = attributes
      .flatMap(_.nestedRecord)
      .map { nested =>
        renderNestedRecordClass(nested, namespace, className)
      }
      .mkString("\n")

    s"""$doc final case class $className(
$fields
) {
  def toCedarValue: CedarValue = CedarValue.record(Map(
$toCedarFields
  ))
}

object $className {
$nestedClasses
}
"""
  }

  private def commonTypeToScalaType(schemaType: SchemaType): String = schemaType match {
    case SchemaType.Primitive(PrimitiveType.String)   => "String"
    case SchemaType.Primitive(PrimitiveType.Long)     => "Long"
    case SchemaType.Primitive(PrimitiveType.Bool)     => "Boolean"
    case SchemaType.Extension(ExtensionType.ipaddr)   => "String"
    case SchemaType.Extension(ExtensionType.decimal)  => "BigDecimal"
    case SchemaType.Extension(ExtensionType.datetime) => "java.time.Instant"
    case SchemaType.Extension(ExtensionType.duration) => "java.time.Duration"
    case SchemaType.EntityRef(_)                      => "String"
    case SchemaType.SetOf(element)                    => s"Set[${commonTypeToScalaType(element)}]"
    case SchemaType.Record(_)                         => "Any"
    case SchemaType.TypeRef(name)                     => escapeIdent(name.simple)
  }

  private def renderEntityClass(entity: EntityIR, ir: CedarIR, namespace: String): String = {
    // Handle enum entities differently - generate a Scala 3 enum
    if (entity.isEnum) {
      return renderEnumEntity(entity, namespace)
    }

    val className = escapeIdent(entity.name) // Just "Xxx"
    val fullEntityType = s"$namespace::${entity.name}"

    // Build field list: ID + parent IDs + attributes
    // All IDs are now newtypes (generated in EntityIds.scala)
    val idType = s"${entity.name}Id"
    val idField = s"    id: $idType"

    // Parent ID fields (immediate parent only, as that's what we need to resolve hierarchy)
    val parentFields = entity.immediateParent.map { parentName =>
      val parentType = s"${parentName}Id" // Generated newtype
      s"    ${parentName.take(1).toLowerCase + parentName.drop(1)}Id: $parentType"
    }.toList

    // Attribute fields - for nested records, use the nested class name qualified with entity name
    val attrFields = entity.attributes.map { attr =>
      val scalaType = attr.nestedRecord match {
        case Some(nested) =>
          // Use the nested class name (it's declared inside the companion object)
          val nestedType = s"$className.${nested.className}"
          if (attr.optional) s"Option[$nestedType]" else nestedType
        case None => attr.scalaType
      }
      s"    ${attr.fieldName}: $scalaType"
    }

    val allFields = (idField :: parentFields ++ attrFields).mkString(",\n")

    val doc = entity.doc.map(d => s"  /** $d */\n").getOrElse("")

    // All IDs are newtypes, so always use .value to extract string
    val idToStringExpr = ".value"

    // Build parent set for Cedar entity
    val parentSet = entity.immediateParent match {
      case Some(parentName) =>
        val parentType = s"$namespace::$parentName"
        val parentIdField = parentName.take(1).toLowerCase + parentName.drop(1) + "Id"
        val quote = '"'
        s"Set(CedarEntityUid($quote$parentType$quote, a.$parentIdField.value))"
      case None =>
        "Set.empty"
    }

    // Build attribute map
    val attrMap = if (entity.attributes.nonEmpty) {
      val attrs = entity.attributes.map { attr =>
        val valueExpr = renderCedarValueExprWithPrefix("a.", attr, namespace, className)
        val quote = '"'
        s"$quote${attr.name}$quote -> $valueExpr"
      }
      s"Map(\n            ${attrs.mkString(",\n            ")}\n          )"
    } else {
      "Map.empty"
    }

    // Build getParentIds
    val parentExtraction = entity.immediateParent match {
      case Some(parentName) =>
        val parentType = s"$namespace::$parentName"
        val parentIdField = parentName.take(1).toLowerCase + parentName.drop(1) + "Id"
        val quote = '"'
        s"List($quote$parentType$quote -> a.$parentIdField.value)"
      case None =>
        "Nil"
    }

    // Generate nested case classes for record attributes
    val nestedClasses = entity.attributes
      .flatMap(_.nestedRecord)
      .map { nested =>
        renderNestedRecordClass(nested, namespace, className)
      }
      .mkString("\n")

    s"""${doc}  final case class $className(
$allFields
  )

  object $className {
$nestedClasses
    /** CedarEntityType instance for automatic conversion and type-safe registration */
    implicit val cedarEntityType: CedarEntityType.Aux[$className, $idType] = new CedarEntityType[$className] {
      type Id = $idType
      val entityType: String = "$fullEntityType"
      
      def toCedarEntity(a: $className): CedarEntity = CedarEntity(
        entityType = entityType,
        entityId = a.id$idToStringExpr,
        parents = $parentSet,
        attributes = $attrMap
      )
      
      def getParentIds(a: $className): List[(String, String)] = $parentExtraction
    }
  }"""
  }

  /** Render a Scala 3 enum for a Cedar enum entity.
    *
    * Cedar enum entities like:
    * {{{
    * entity Status enum ["draft", "published", "archived"];
    * }}}
    *
    * Are rendered as Scala 3 enums:
    * {{{
    * enum Status(val value: String) {
    *   case Draft extends Status("draft")
    *   case Published extends Status("published")
    *   case Archived extends Status("archived")
    * }
    * }}}
    */
  private def renderEnumEntity(entity: EntityIR, namespace: String): String = {
    val className = escapeIdent(entity.name)
    val fullEntityType = s"$namespace::${entity.name}"
    val doc = entity.doc.map(d => s"  /** $d */\n").getOrElse("")

    val enumValues = entity.enumValues.getOrElse(Nil)

    // Generate enum cases - convert value to PascalCase for the case name
    val cases = enumValues.map { value =>
      val caseName = toPascalCaseEnum(value)
      s"""    case $caseName extends $className("$value")"""
    }.mkString("\n")

    // Generate fromString method for parsing
    val fromStringCases = enumValues.map { value =>
      val caseName = toPascalCaseEnum(value)
      s"""      case "$value" => Some($caseName)"""
    }.mkString("\n")

    s"""${doc}  enum $className(val value: String) {
$cases

    /** Convert to Cedar entity UID */
    def toCedarEntityUid: CedarEntityUid = CedarEntityUid("$fullEntityType", value)
  }

  object $className {
    /** Parse from string value */
    def fromString(s: String): Option[$className] = s match {
$fromStringCases
      case _ => None
    }

    /** All enum values */
    val values: List[$className] = List(${enumValues.map(v => toPascalCaseEnum(v)).mkString(", ")})

    /** The Cedar entity type */
    val entityType: String = "$fullEntityType"
  }"""
  }

  /** Convert a string to PascalCase for enum case names.
    * E.g., "draft" -> "Draft", "in-progress" -> "InProgress", "PUBLISHED" -> "Published"
    */
  private def toPascalCaseEnum(value: String): String = {
    value.split("[-_]").map(_.toLowerCase.capitalize).mkString
  }

  /** Render a nested case class for a record attribute.
    */
  private def renderNestedRecordClass(nested: NestedRecordIR, namespace: String, parentClassName: String): String = {
    val fields = nested.attributes
      .map { attr =>
        s"      ${attr.fieldName}: ${attr.scalaType}"
      }
      .mkString(",\n")

    s"""
    /** Nested record type for ${nested.className.head.toLower + nested.className.tail} attribute */
    final case class ${nested.className}(
$fields
    ) {
      /** Convert to Cedar record value */
      def toCedarValue: CedarValue = CedarValue.record(Map(
${nested.attributes
        .map { attr =>
          val valueExpr = renderCedarValueExpr(attr, attr.fieldName, namespace)
          val quote = '"'
          s"        $quote${attr.name}$quote -> $valueExpr"
        }
        .mkString(",\n")}
      ))
    }
"""
  }

  /** Render the CedarValue expression for an attribute with a prefix */
  private def renderCedarValueExprWithPrefix(
      prefix: String,
      attr: AttributeIR,
      namespace: String,
      parentClassName: String = ""
  ): String = {
    val fieldAccess = s"$prefix${attr.fieldName}"

    renderCedarValueExpr(attr, fieldAccess, namespace)
  }

  private def renderCedarValueExpr(attr: AttributeIR, fieldAccess: String, namespace: String): String = {
    if (attr.optional) {
      val valueExpr = renderCedarValueForType(attr.resolvedType, "value", namespace)
      val defaultExpr = renderCedarDefaultValue(attr.resolvedType, namespace)
      s"$fieldAccess.map(value => $valueExpr).getOrElse($defaultExpr)"
    } else {
      renderCedarValueForType(attr.resolvedType, fieldAccess, namespace)
    }
  }

  private def renderCedarValueForType(schemaType: SchemaType, access: String, namespace: String): String =
    schemaType match {
      case SchemaType.Primitive(PrimitiveType.String)   => s"CedarValue.string($access)"
      case SchemaType.Primitive(PrimitiveType.Long)     => s"CedarValue.long($access)"
      case SchemaType.Primitive(PrimitiveType.Bool)     => s"CedarValue.bool($access)"
      case SchemaType.Extension(ExtensionType.ipaddr)   => s"CedarValue.ipaddr($access)"
      case SchemaType.Extension(ExtensionType.decimal)  => s"CedarValue.decimal($access)"
      case SchemaType.Extension(ExtensionType.datetime) => s"CedarValue.datetime($access)"
      case SchemaType.Extension(ExtensionType.duration) => s"CedarValue.duration($access)"
      case SchemaType.EntityRef(name)                   =>
        val fullEntityType = fullEntityTypeName(name, namespace)
        val quote = '"'
        s"CedarValue.entity($quote$fullEntityType$quote, $access)"
      case SchemaType.SetOf(elementType) =>
        elementType match {
          case SchemaType.EntityRef(name) =>
            val fullEntityType = fullEntityTypeName(name, namespace)
            val quote = '"'
            s"CedarValue.entitySet($access, $quote$fullEntityType$quote)"
          case SchemaType.Primitive(PrimitiveType.String) =>
            s"CedarValue.stringSet($access)"
          case SchemaType.Primitive(PrimitiveType.Long) =>
            s"CedarValue.set($access.map(CedarValue.long))"
          case SchemaType.Primitive(PrimitiveType.Bool) =>
            s"CedarValue.set($access.map(CedarValue.bool))"
          case SchemaType.Extension(ExtensionType.ipaddr) =>
            s"CedarValue.set($access.map(CedarValue.ipaddr))"
          case SchemaType.Extension(ExtensionType.decimal) =>
            s"CedarValue.set($access.map(CedarValue.decimal))"
          case SchemaType.Extension(ExtensionType.datetime) =>
            s"CedarValue.set($access.map(CedarValue.datetime))"
          case SchemaType.Extension(ExtensionType.duration) =>
            s"CedarValue.set($access.map(CedarValue.duration))"
          case SchemaType.Record(_) =>
            s"CedarValue.set($access.map(_.toCedarValue))"
          case SchemaType.TypeRef(_) =>
            // TypeRef should normally be resolved before reaching here.
            // If we hit this, it means the type couldn't be resolved.
            s"CedarValue.set($access.map(v => CedarValue.string(v.toString)))"
          case SchemaType.SetOf(inner) =>
            // Nested sets are unusual but handle gracefully
            val innerExpr = renderCedarValueForType(inner, "v", namespace)
            s"CedarValue.set($access.map(v => $innerExpr))"
        }
      case SchemaType.Record(_) =>
        s"$access.toCedarValue"
      case SchemaType.TypeRef(_) =>
        s"CedarValue.string($access.toString)"
    }

  private def renderCedarDefaultValue(schemaType: SchemaType, namespace: String): String = schemaType match {
    case SchemaType.Primitive(PrimitiveType.String)   => """CedarValue.string("")"""
    case SchemaType.Primitive(PrimitiveType.Long)     => "CedarValue.long(0)"
    case SchemaType.Primitive(PrimitiveType.Bool)     => "CedarValue.bool(false)"
    case SchemaType.Extension(ExtensionType.ipaddr)   => """CedarValue.ipaddr("")"""
    case SchemaType.Extension(ExtensionType.decimal)  => "CedarValue.decimal(BigDecimal(0))"
    case SchemaType.Extension(ExtensionType.datetime) => "CedarValue.datetime(java.time.Instant.EPOCH)"
    case SchemaType.Extension(ExtensionType.duration) => "CedarValue.duration(java.time.Duration.ZERO)"
    case SchemaType.EntityRef(name)                   =>
      val fullEntityType = fullEntityTypeName(name, namespace)
      val quote = '"'
      s"CedarValue.entity($quote$fullEntityType$quote, $quote$quote)"
    case SchemaType.SetOf(elementType) =>
      elementType match {
        case SchemaType.EntityRef(name) =>
          val fullEntityType = fullEntityTypeName(name, namespace)
          val quote = '"'
          s"CedarValue.entitySet(Set.empty, $quote$fullEntityType$quote)"
        case SchemaType.Primitive(PrimitiveType.String) =>
          "CedarValue.stringSet(Set.empty)"
        case _ =>
          "CedarValue.set(Set.empty[CedarValue])"
      }
    case SchemaType.Record(_) =>
      "CedarValue.record(Map.empty)"
    case SchemaType.TypeRef(_) =>
      """CedarValue.string("")"""
  }

  private def fullEntityTypeName(name: QualifiedName, namespace: String): String = {
    if (name.value.contains("::")) name.value else s"$namespace::${name.simple}"
  }

  // ============================================================================
  // EntityFetcher Type Aliases
  // ============================================================================

  private def renderEntityFetchers(ir: CedarIR, pkg: String, dslName: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))

    val imports = s"""import cedar4s.entities.EntityFetcher"""

    val typeAliases = entities
      .map { entity =>
        renderEntityFetcherAlias(entity, dslName)
      }
      .mkString("\n\n")

    s"""$header
$imports

/**
 * Type aliases for entity fetchers.
 * 
 * These type aliases provide convenient naming for fetcher implementations
 * with typed IDs:
 * 
 * {{{
 * // Instead of EntityFetcher[Future, Document, DocumentId]
 * class DocumentFetcherImpl extends DocumentFetcher[Future] { ... }
 * 
 * // Or use the type alias explicitly
 * val fetcher: DocumentFetcher[Future] = new EntityFetcher[Future, Document, DocumentId] { ... }
 * }}}
 * 
 * == Implementation ==
 * 
 * Implement an EntityFetcher that maps your domain model to the generated entity class:
 * 
 * {{{
 * class DocumentFetcherImpl(db: Database)(implicit ec: ExecutionContext) 
 *     extends DocumentFetcher[Future] {
 *   
  *   // id is typed as a newtype (e.g., DocumentId)
  *   def fetch(id: DocumentId): Future[Option[Document]] =
 *     db.run(docDao.get(id)).map(_.map { doc =>
 *       Document(id = doc.id, folderId = doc.folderId, ...)
 *     })
 * }
 * }}}
 */
$typeAliases
"""
  }

  private def renderEntityFetcherAlias(entity: EntityIR, dslName: String): String = {
    val aliasName = s"${entity.name}Fetcher"
    val entityClass = s"$dslName.Entity.${escapeIdent(entity.name)}" // Reference the nested entity class
    val idType = s"${entity.name}Id" // Generated newtype

    s"""/** Fetcher for ${entity.name} entities */
type $aliasName[F[_]] = EntityFetcher[F, $entityClass, $idType]"""
  }

  // ============================================================================
  // EntitySchema Metadata
  // ============================================================================

  private def renderEntitySchema(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))
    val namespace = ir.namespace

    val imports = s"""import cedar4s.schema.OwnershipType"""

    // Build entity metadata entries
    val entityEntries = entities
      .map { e =>
        val parentChainStr =
          if (e.parentChain.isEmpty) "Nil"
          else s"""List(${e.parentChain.map(p => s""""$p"""").mkString(", ")})"""

        s"""    "${e.name}" -> EntityMetadata(
      entityType = "$namespace::${e.name}",
      ownership = OwnershipType.${ownershipTypeName(e.ownership)},
      parentChain = $parentChainStr,
      depth = ${e.depth}
    )"""
      }
      .mkString(",\n")

    // Build hierarchy entries (parent -> children mapping)
    val childrenByParent = entities.groupBy(_.immediateParent)
    val hierarchyEntries = childrenByParent
      .collect { case (Some(parent), children) =>
        s"""    "$parent" -> Set(${children.map(c => s""""${c.name}"""").mkString(", ")})"""
      }
      .mkString(",\n")

    s"""$header
$imports

/**
 * Schema metadata for entity types.
 * 
 * Provides runtime access to entity hierarchy information
 * derived from the Cedar schema.
 */
object EntitySchema {
  
  /** The Cedar namespace for this schema */
  val namespace: String = "$namespace"
  
  /**
   * Metadata for a single entity type.
   */
  final case class EntityMetadata(
      entityType: String,
      ownership: OwnershipType,
      parentChain: List[String],
      depth: Int
  ) {
    def isRoot: Boolean = ownership == OwnershipType.Root
    def immediateParent: Option[String] = parentChain.lastOption
  }
  
  /**
   * Metadata for all entity types.
   */
  val entities: Map[String, EntityMetadata] = Map(
$entityEntries
  )
  
  /**
   * Parent -> children mapping for hierarchy traversal.
   */
  val children: Map[String, Set[String]] = Map(
$hierarchyEntries
  )
  
  /**
   * Root entity types (no parents).
   */
  val roots: Set[String] = entities.values.filter(_.isRoot).map(_.entityType.split("::").last).toSet
  
  /**
   * Get entity metadata by name.
   */
  def get(name: String): Option[EntityMetadata] = entities.get(name)
  
  /**
   * Get full Cedar entity type from simple name.
   */
  def fullEntityType(name: String): String = s"$namespace::$$name"
  
  /**
   * Get children of an entity type.
   */
  def childrenOf(name: String): Set[String] = children.getOrElse(name, Set.empty)
}
"""
  }

  // ============================================================================
  // Principals - Type-safe principal types
  // ============================================================================

  private def renderPrincipals(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val namespace = ir.namespace
    val entitiesByName = ir.entitiesByName

    // Get all unique principal types from actions
    val principalTypes = ir.allPrincipalTypes.toList.sorted

    if (principalTypes.isEmpty) {
      // No principal types defined - generate empty object
      return s"""$header
import cedar4s.auth.Principal
import cedar4s.schema.CedarEntityUid

/**
 * Principal types for Cedar authorization.
 * 
 * No principal types were defined in the schema's action declarations.
 * Add `principal: [...]` clauses to your action declarations to generate
 * type-safe principal classes.
 */
object Principals {
  // No principal types defined in schema
}
"""
    }

    // ID types come from EntityIds - no additional imports needed
    val imports = s"""import cedar4s.auth.Principal
import cedar4s.schema.CedarEntityUid"""

    val principalClasses = principalTypes
      .map { principalType =>
        renderPrincipalClass(principalType, ir, namespace)
      }
      .mkString("\n")

    // Generate a sealed trait for type-safe principal matching
    val traitCases = principalTypes.map(p => s"    case class ${p}(id: String) extends PrincipalType").mkString("\n")

    s"""$header
$imports

/**
 * Principal types for Cedar authorization.
 * 
 * These case classes represent the principal types that can perform actions
 * in this Cedar schema. Use these with `.asPrincipal()` for explicit principal
 * specification, or rely on the default principal from `AuthRunner`.
 * 
 * == Example ==
 * 
 * {{{
 * import $pkg.Principals._
 * 
 * // Explicit principal (checked at compile-time via CanPerform evidence)
 * Document.View("folder-1", "doc-1")
 *   .asPrincipal(User(UserId("alice")))
 *   .require
 * 
 * // ServiceAccount principal
 * Project.View(orgId, workspaceId, projectId)
 *   .asPrincipal(ServiceAccount(ServiceAccountId("ci-bot")))
 *   .require
 * }}}
 */
object Principals {
$principalClasses
}
"""
  }

  private def renderPrincipalClass(
      principalType: String,
      ir: CedarIR,
      namespace: String
  ): String = {
    val entity = ir.entitiesByName.get(principalType)

    // All principal IDs are newtypes
    val idType = s"${principalType}Id"

    // Entity UID construction - use .value since it's a newtype
    val entityUidExpr = s"""CedarEntityUid("$namespace::$principalType", id.value)"""

    val doc = entity.flatMap(_.doc).map(d => s"  /** $d */\n").getOrElse("")

    s"""
$doc  /**
   * $principalType principal type.
   * 
   * Entity type: $namespace::$principalType
   */
  final case class ${escapeIdent(principalType)}(id: $idType) extends Principal {
    val entityType: String = "$namespace::$principalType"
    val entityId: String = id.value
    override def toCedarEntity: CedarEntityUid = $entityUidExpr
  }"""
  }

  // ============================================================================
  // PrincipalEvidence - Compile-time principal/action validation
  // ============================================================================

  private def renderPrincipalEvidence(ir: CedarIR, pkg: String, dslName: String): String = {
    val header = fileHeader(pkg)
    val domains = SchemaToIR.buildDomains(ir)

    // Collect all (principal, action) pairs
    val evidencePairs = ir.actions
      .flatMap { action =>
        action.principalTypes.map(p => (p, action.domain, action.objectName))
      }
      .distinct
      .sortBy { case (p, d, a) => (p, d, a) }

    if (evidencePairs.isEmpty) {
      return s"""$header
import cedar4s.auth.{CedarAction, Principal}
import cedar4s.auth.Principal.CanPerform

/**
 * Compile-time evidence for principal/action relationships.
 * 
 * No principal types were defined in the schema's action declarations.
 * Import [[cedar4s.auth.Principal.CanPerform.anyPrincipalCanPerformAnyAction]]
 * to disable compile-time principal checking.
 */
object PrincipalEvidence {
  // No evidence generated - no principal types defined in schema
}
"""
    }

    val imports = s"""import cedar4s.auth.{CedarAction, Principal}
import cedar4s.auth.Principal.CanPerform"""

    // Group evidence by principal for readability
    val evidenceByPrincipal = evidencePairs.groupBy(_._1).toList.sortBy(_._1)

    val evidenceInstances = evidenceByPrincipal
      .map { case (principal, pairs) =>
        val instances = pairs
          .map { case (_, domain, action) =>
            // Include domain in the name to avoid conflicts when multiple domains have same action names
            val givenName = s"canPerform_${principal}_${domain}_$action"
            val principalName = escapeIdent(principal)
            val domainName = escapeIdent(domain)
            s"  implicit val $givenName: CanPerform[$dslName.Principal.$principalName, $dslName.Actions.$domainName.${action}.type] = new CanPerform[$dslName.Principal.$principalName, $dslName.Actions.$domainName.${action}.type] {}"
          }
          .mkString("\n")

        s"""
  // $principal can perform:
$instances"""
      }
      .mkString("\n")

    s"""$header
$imports

/**
 * Compile-time evidence for principal/action relationships.
 *
 * These implicit instances enable compile-time checking that a principal type
 * is allowed to perform a specific action. Attempting to use `.asPrincipal()`
 * with a principal type that isn't allowed for an action will result in a
 * compilation error.
 *
 * Generated from the `principal: [...]` clauses in the Cedar schema's action
 * declarations.
 *
 * == Example ==
 *
 * Given this schema:
 * {{{
 * action "Document::edit" appliesTo {
 *     principal: [User],  // Only User can edit
 *     resource: Document,
 * };
 * }}}
 *
 * This compiles:
 * {{{
 * Document.Edit(folderId, docId).asPrincipal(User("alice"))  // OK
 * }}}
 *
 * This fails at compile time:
 * {{{
 * Document.Edit(folderId, docId).asPrincipal(ServiceAccount("bot"))  // Error!
 * // error: could not find implicit value for evidence parameter
 * }}}
 */
object PrincipalEvidence {
$evidenceInstances
}
"""
  }

  // ============================================================================
  // Contexts - Type-safe context types for actions
  // ============================================================================

  private def renderContexts(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)

    // Find all actions that have context attributes defined
    val actionsWithContext = ir.actions.filter(_.contextAttributes.nonEmpty)

    if (actionsWithContext.isEmpty) {
      return s"""$header
import cedar4s.auth.ContextSchema
import cedar4s.entities.CedarValue

/**
 * Context types for Cedar authorization.
 * 
 * No context schemas were defined in the Cedar schema's action declarations.
 * Add `context: { ... }` blocks to your action declarations to generate
 * type-safe context classes.
 */
object Contexts {
  // No context types defined in schema
}
"""
    }

    val imports = s"""import cedar4s.auth.ContextSchema
import cedar4s.entities.CedarValue"""

    val contextClasses = actionsWithContext
      .map { action =>
        renderContextClass(action, ir.namespace)
      }
      .mkString("\n")

    s"""$header
$imports

/**
 * Context types for Cedar authorization.
 * 
 * These case classes represent the context schemas defined in Cedar action
 * declarations. Use these with `.withContext()` for type-safe context.
 * 
 * == Example ==
 * 
 * {{{
 * import $pkg.Contexts._
 * import $pkg.ActionDsl._
 * import $pkg.Actions._
 * 
 * // Create a check with typed context
 * Document.View(folderId, docId)
 *   .withContext(DocumentViewContext()
 *     .withRequestTime(System.currentTimeMillis())
 *     .withSourceIP("192.168.1.1")
 *   )
 *   .require
 * }}}
 */
object Contexts {
$contextClasses
}
"""
  }

  private def renderContextClass(action: ActionIR, namespace: String): String = {
    // Class name: combine domain + action name + "Context"
    // e.g., "Document" + "view" -> "DocumentViewContext"
    val className = s"${action.domain}${action.objectName}Context"

    // Helper to convert Cedar attribute names to Scala field names (camelCase)
    def toFieldName(name: String): String = {
      val parts = name.split("[-_]")
      if (parts.length == 1) {
        // Already single word, just lowercase first char
        name.head.toLower + name.tail
      } else {
        // Convert to camelCase
        parts.zipWithIndex.map { case (part, i) =>
          if (i == 0) part.toLowerCase else part.capitalize
        }.mkString
      }
    }

    // Helper to capitalize first letter for builder method names
    def capitalize(name: String): String =
      if (name.isEmpty) name else name.head.toUpper + name.tail

    // Fields - all Optional to allow builder pattern with defaults
    val fields = action.contextAttributes
      .map { attr =>
        val fieldName = toFieldName(attr.name)
        val optType = if (attr.optional) attr.scalaType else s"Option[${attr.scalaType}]"
        s"    $fieldName: $optType = None"
      }
      .mkString(",\n")

    // Builder methods
    val builders = action.contextAttributes
      .map { attr =>
        val fieldName = toFieldName(attr.name)
        val methodName = capitalize(fieldName)
        val paramType = attr.scalaType.stripPrefix("Option[").stripSuffix("]")
        val actualParamType = if (attr.optional) paramType.stripPrefix("Option[").stripSuffix("]") else paramType
        s"""
    /** Set the ${attr.name} context attribute */
    def with$methodName(value: $actualParamType): $className = 
      copy($fieldName = Some(value))"""
      }
      .mkString("\n")

    // toMap implementation - convert each field to CedarValue
    val toMapEntries = action.contextAttributes
      .map { attr =>
        val fieldName = toFieldName(attr.name)
        val valueExpr = renderCedarValueForType(attr.resolvedType, "v", namespace)
        s"""      $fieldName.map(v => "${attr.name}" -> $valueExpr)"""
      }
      .mkString(",\n")

    s"""
  /**
   * Context for ${action.domain}::${action.name} action.
   * 
   * Cedar context schema: { ${action.contextAttributes.map(a => s"${a.name}: ${a.cedarType}").mkString(", ")} }
   */
  final case class $className(
$fields
  ) extends ContextSchema {
$builders

    /** Convert to Cedar context map */
    def toMap: Map[String, CedarValue] = List(
$toMapEntries
    ).flatten.toMap
  }"""
  }

  // ============================================================================
  // HasParentEvidence - Type-safe parent access
  // ============================================================================

  private def renderHasParentEvidence(ir: CedarIR, pkg: String): String = {
    val header = fileHeader(pkg)
    val entities = ir.entities.sortBy(e => (e.depth, e.name))

    // Build (child, parent) pairs from entity hierarchies
    // Each entity in parentChain is a direct or transitive parent
    val parentPairs = entities
      .flatMap { entity =>
        // Direct parent is last in chain
        entity.immediateParent.toList.map(parent => (entity.name, parent, "direct")) ++
          // All ancestors are transitive parents
          entity.parentChain.dropRight(1).map(parent => (entity.name, parent, "transitive"))
      }
      .distinct
      .sortBy { case (child, parent, _) => (child, parent) }

    if (parentPairs.isEmpty) {
      return s"""$header
import cedar4s.auth.{HasParent, DomainTypeTag}

/**
 * Type-safe parent relationship evidence.
 * 
 * No parent relationships found - all entities are root entities.
 */
object HasParentEvidence {
  // No parent relationships in schema
}
"""
    }

    val imports = s"""import cedar4s.auth.{HasParent, DomainTypeTag}"""

    // Generate HasParent instances
    val hasParentInstances = parentPairs
      .map { case (child, parent, relationship) =>
        val givenName = s"${child.toLowerCase}Has${parent}Parent"
        s"""
  /** $child ${if (relationship == "direct") "is directly in" else "is transitively in"} $parent */
  implicit val $givenName: HasParent[PolicyDomain.${escapeIdent(child)}.type, PolicyDomain.${escapeIdent(
            parent
          )}.type] = new HasParent[PolicyDomain.${escapeIdent(child)}.type, PolicyDomain.${escapeIdent(
            parent
          )}.type] {}"""
      }
      .mkString("\n")

    // Generate DomainTypeTag instances for runtime type matching
    // Note: We use anonymous class syntax instead of DomainTypeTag.instance()
    // to preserve the specific singleton type in the given definition
    val typeTagInstances = entities
      .map { entity =>
        val givenName = s"${entity.name.toLowerCase}TypeTag"
        s"""  implicit val $givenName: DomainTypeTag[PolicyDomain.${escapeIdent(
            entity.name
          )}.type] = new DomainTypeTag[PolicyDomain.${escapeIdent(
            entity.name
          )}.type] { val typeName = "${entity.name}" }"""
      }
      .mkString("\n")

    s"""$header
$imports

/**
 * Type-safe parent relationship evidence.
 *
 * These implicit instances enable compile-time checked parent access on resources:
 *
 * {{{
 * import $pkg.HasParentEvidence._
 * import $pkg.Resource
 *
 * val docResource: Resource[PolicyDomain.Document.type, DirectOwnership] = ...
 *
 * // Compiles - Document is in Folder
 * val folderId: Option[String] = docResource.parent[PolicyDomain.Folder.type]
 *
 * // Won't compile - Document is not in User
 * // val userId = docResource.parent[PolicyDomain.User.type]  // Error!
 * }}}
 *
 * Generated from the `entity X in [Y, Z]` declarations in the Cedar schema.
 */
object HasParentEvidence {
  // HasParent instances
$hasParentInstances

  // DomainTypeTag instances for runtime type matching
$typeTagInstances
}
"""
  }

  // ============================================================================
  // Helper methods to extract object content for nested DSL objects
  // ============================================================================

  private def renderActionsObjectContent(ir: CedarIR, namespace: String): String = {
    // Use buildDomainsByActionDomain for Actions - group by action name prefix
    val domains = SchemaToIR.buildDomainsByActionDomain(ir)

    val domainObjects = domains
      .map { d =>
        renderDomainActions(d, namespace)
      }
      .mkString("\n")

    val allActions = domains
      .flatMap(_.actions)
      .map { a =>
        s"${a.domain}.${a.objectName}"
      }
      .mkString(", ")

    s"""
  /**
   * Base trait for all Cedar actions.
   *
   * Actions have phantom type members to enable compile-time checking
   * that action/resource pairs are compatible:
   * - Domain: The policy domain (e.g., PolicyDomain.Mission.type)
   * - Ownership: The ownership classification (RootOwnership, DirectOwnership, IndirectOwnership)
   *
   * Extends [[CedarAction]] from cedar4s-core for compatibility with the auth DSL.
   */
  object Actions {

    /**
     * Base trait for all Cedar actions.
     *
     * Actions have phantom type members to enable compile-time checking
     * that action/resource pairs are compatible:
     * - Domain: The policy domain (e.g., PolicyDomain.Mission.type)
     * - Ownership: The ownership classification (RootOwnership, DirectOwnership, IndirectOwnership)
     *
     * Extends [[CedarAction]] from cedar4s-core for compatibility with the auth DSL.
     */
    sealed trait Action extends CedarAction {
      /** Phantom type for the policy domain */
      type Domain <: PolicyDomain

      /** Phantom type for ownership classification */
      type Ownership

      /** Backward-compatible alias for name */
      def value: String = name

      /** Domain this action belongs to */
      def domain: PolicyDomain
    }
$domainObjects

    /** All actions across all domains */
    val all: Set[Action] = Set($allActions)

    /** Find action by Cedar action string */
    def fromCedarAction(cedarAction: String): Option[Action] =
      all.find(_.cedarAction == cedarAction)

    /** Find action by domain and simple name */
    def fromString(domain: PolicyDomain, name: String): Option[Action] =
      all.find(a => a.domain == domain && a.name == name)

    /**
     * Get all action names for a specific domain.
     *
     * Useful for capability checks where you need to test all possible
     * actions for a domain.
     *
     * @param domain The policy domain to get actions for
     * @return Set of action names (e.g., Set("create", "read", "update"))
     */
    def allForDomain(domain: PolicyDomain): Set[String] = domain match {
${domains
        .map(d => s"""      case PolicyDomain.${escapeIdent(d.name)} => ${escapeIdent(d.name)}.all.map(_.name)""")
        .mkString("\n")}
    }
  }"""
  }

  private def renderPrincipalObjectContent(ir: CedarIR, namespace: String): String = {
    val entitiesByName = ir.entitiesByName

    // Get all unique principal types from actions
    val principalTypes = ir.allPrincipalTypes.toList.sorted

    if (principalTypes.isEmpty) {
      // No principal types defined - generate empty object
      return s"""
  /**
   * Principal types for Cedar authorization.
   *
   * No principal types were defined in the schema's action declarations.
   * Add `principal: [...]` clauses to your action declarations to generate
   * type-safe principal classes.
   */
  object Principal {
    // No principal types defined in schema
  }"""
    }

    val principalClasses = principalTypes
      .map { principalType =>
        val entity = ir.entitiesByName.get(principalType)

        // All principal IDs are newtypes
        val idType = s"${principalType}Id"

        // Entity UID construction - use .value since it's a newtype
        val entityUidExpr = s"""CedarEntityUid("$namespace::$principalType", id.value)"""

        val doc = entity.flatMap(_.doc).map(d => s"    /** $d */\n").getOrElse("")

        s"""
$doc    /**
     * $principalType principal type.
     *
     * Entity type: $namespace::$principalType
     */
    final case class ${escapeIdent(principalType)}(id: $idType) extends Principal {
      val entityType: String = "$namespace::$principalType"
      val entityId: String = id.value
      override def toCedarEntity: CedarEntityUid = $entityUidExpr
    }"""
      }
      .mkString("\n")

    s"""
  /**
   * Principal types for Cedar authorization.
   *
   * These case classes represent the principal types that can perform actions
   * in this Cedar schema. Use these with `.asPrincipal()` for explicit principal
   * specification, or rely on the default principal from `CedarSession`.
   */
  object Principal {
$principalClasses
  }"""
  }

  private def renderResourceObjectContent(ir: CedarIR, pkg: String, namespace: String): String = {
    val entities = ir.entities.sortBy(e => (e.depth, e.name))
    val pkgLastSegment = pkg.split("\\.").last

    // Generate ownership marker traits
    val ownershipTraits = s"""
    /**
     * Phantom type markers for resource ownership classification.
     * These are sealed traits used only at the type level to enforce
     * that actions are called with correctly-shaped resources.
     */
    sealed trait RootOwnership
    sealed trait DirectOwnership
    sealed trait IndirectOwnership

    // Backward-compatible type aliases
    type TenantOwnership = RootOwnership
    type LocationOwnership = IndirectOwnership
"""

    // Generate the Resource case class
    val resourceClass = s"""
    /**
     * Type-safe resource wrapper for Cedar authorization.
     *
     * This wrapper contains all the information needed to:
     * 1. Create Cedar entity UIDs for authorization requests
     * 2. Build entity hierarchies for parent chain traversal
     *
     * Extends [[CedarResource]] from cedar4s-core for compatibility with the auth DSL.
     *
     * @tparam D The domain phantom type (e.g., PolicyDomain.Mission.type)
     * @tparam O The ownership phantom type marker (e.g., DirectOwnership)
     */
    final case class Resource[D <: PolicyDomain, O] private[$pkgLastSegment] (
        entityType: String,
        entityId: Option[String],
        parents: List[(String, String)]
    ) extends CedarResource {

      /** Backward-compatible alias for entityId */
      def resourceId: Option[String] = entityId

      /** Format as Cedar entity UID */
      def toCedarEntityUid: CedarEntityUid = {
        // entityType already includes namespace (e.g., "DocShare::Document")
        entityId match {
          case Some(id) => CedarEntityUid(entityType, id)
          case None =>
            // For collection contexts, use the immediate parent
            parents.lastOption match {
              case Some((parentType, parentId)) => CedarEntityUid(parentType, parentId)
              case None => CedarEntityUid(entityType, "unknown")
            }
        }
      }

      /** Format as Cedar entity UID string */
      def toCedarEntity: String = entityId match {
        case Some(id) => s\"\"\"$$entityType::"$$id"\"\"\"
        case None =>
          // For collection contexts, use the immediate parent
          parents.lastOption match {
            case Some((parentType, parentId)) => s\"\"\"$$parentType::"$$parentId"\"\"\"
            case None => entityType
          }
      }

      /** Get parent entity UIDs for Cedar context */
      def parentEntities: List[String] =
        parents.map { case (t, id) => s\"\"\"$$t::"$$id"\"\"\" }

      /** Get the customer ID from parents (if present) */
      def customerId: Option[String] = entityId.filter(_ => entityType == "$namespace::Customer")
        .orElse(parents.collectFirst { case ("$namespace::Customer", id) => id })

      /** Get the location ID from parents (if present) */
      def locationId: Option[String] = entityId.filter(_ => entityType == "$namespace::Location")
        .orElse(parents.collectFirst { case ("$namespace::Location", id) => id })

      /** Get the domain as PolicyDomain */
      def domain: PolicyDomain = PolicyDomain.values.find(d => entityType.endsWith("::" + d.entityType) || entityType == d.entityType).get
    }"""

    s"""
  /**
   * Resource types and factory methods.
   */
  object Resource {
$ownershipTraits
$resourceClass

    /**
      * Low-level constructor for dynamic resource creation.
      *
      * Use this when you need to construct resources dynamically at runtime
      * without knowing the concrete types at compile time.
      *
      * For normal authorization checks, use the DSL's `.on(id)` pattern which
      * automatically resolves the resource hierarchy via EntityStore.
      */
    def apply[D <: PolicyDomain, O](
        entityType: String,
        entityId: Option[String],
        parents: List[(String, String)]
    ): Resource[D, O] = new Resource(entityType, entityId, parents)

    /**
      * Construct a Resource from a ResourceRef.
      *
      * Used by the deferred `.on(id)` pattern when resolving resources
      * via EntityStore.
      */
    def fromResourceRef[D <: PolicyDomain, O](ref: ResourceRef): Resource[D, O] = {
      // ResourceRef already has namespace-prefixed entity types
      new Resource(ref.entityType, ref.entityId, ref.parents)
    }
  }"""
  }

  private def renderEntityObjectContent(ir: CedarIR, namespace: String): String = {
    val entities = ir.entities.sortBy(e => (e.depth, e.name))

    // Identify principal entity types
    val principalTypes = ir.allPrincipalTypes

    val entityClasses = entities
      .map { entity =>
        // Handle enum entities differently - generate a Scala 3 enum
        if (entity.isEnum) {
          renderEnumEntityForDsl(entity, namespace)
        } else {
          renderRegularEntityForDsl(entity, namespace, principalTypes)
        }
      }
      .mkString("\n\n")

    // Generate PrincipalEntity trait if there are any principals
    val principalTrait = if (principalTypes.nonEmpty) {
      """
    /**
     * Marker trait for entities that can be principals.
     *
     * Only entities used in the principal position of actions extend this trait.
     * This enables type-safe principal resolution where the compiler ensures
     * only valid principal entities can be returned from PrincipalResolver.
     */
    sealed trait PrincipalEntity
"""
    } else {
      ""
    }

    s"""
  /**
   * Entity classes for Cedar authorization.
   *
   * Each case class has a companion object with a [[CedarEntityType]] instance
   * that enables automatic conversion to Cedar entities and type-safe registration
   * with [[cedar4s.entities.EntityStore]].
   */
  object Entity {$principalTrait
$entityClasses
  }"""
  }

  /** Render a Scala 3 enum entity for the DSL.
    */
  private def renderEnumEntityForDsl(entity: EntityIR, namespace: String): String = {
    val className = escapeIdent(entity.name)
    val fullEntityType = s"$namespace::${entity.name}"
    val doc = entity.doc.map(d => s"    /** $d */\n").getOrElse("")

    val enumValues = entity.enumValues.getOrElse(Nil)

    // Generate enum cases - convert value to PascalCase for the case name
    val cases = enumValues.map { value =>
      val caseName = toPascalCaseEnum(value)
      s"""      case $caseName extends $className("$value")"""
    }.mkString("\n")

    // Generate fromString method for parsing
    val fromStringCases = enumValues.map { value =>
      val caseName = toPascalCaseEnum(value)
      s"""        case "$value" => Some($caseName)"""
    }.mkString("\n")

    s"""${doc}    enum $className(val value: String) {
$cases

      /** Convert to Cedar entity UID */
      def toCedarEntityUid: CedarEntityUid = CedarEntityUid("$fullEntityType", value)
    }

    object $className {
      /** Parse from string value */
      def fromString(s: String): Option[$className] = s match {
$fromStringCases
        case _ => None
      }

      /** All enum values */
      val values: List[$className] = List(${enumValues.map(v => toPascalCaseEnum(v)).mkString(", ")})

      /** The Cedar entity type */
      val entityType: String = "$fullEntityType"
    }"""
  }

  /** Render a regular (non-enum) entity for the DSL.
    */
  private def renderRegularEntityForDsl(entity: EntityIR, namespace: String, principalTypes: Set[String]): String = {
    val className = escapeIdent(entity.name) // Just "Xxx"
    val fullEntityType = s"$namespace::${entity.name}"
    val isPrincipal = principalTypes.contains(entity.name)

    // Build field list: ID + parent IDs + attributes
    // All IDs are now newtypes (generated in EntityIds.scala)
    val idType = s"${entity.name}Id"
    val idField = s"      id: $idType"

    // Parent ID fields (immediate parent only, as that's what we need to resolve hierarchy)
    val parentFields = entity.immediateParent.map { parentName =>
      val parentType = s"${parentName}Id" // Generated newtype
      s"      ${parentName.take(1).toLowerCase + parentName.drop(1)}Id: $parentType"
    }.toList

    // Attribute fields - for nested records, use the nested class name qualified with entity name
    val attrFields = entity.attributes.map { attr =>
      val scalaType = attr.nestedRecord match {
        case Some(nested) =>
          // Use the nested class name (it's declared inside the companion object)
          val nestedType = s"$className.${nested.className}"
          if (attr.optional) s"Option[$nestedType]" else nestedType
        case None => attr.scalaType
      }
      s"      ${attr.fieldName}: $scalaType"
    }

    val allFields = (idField :: parentFields ++ attrFields).mkString(",\n")

    val doc = entity.doc.map(d => s"    /** $d */\n").getOrElse("")

    // Add PrincipalEntity trait extension if this is a principal
    val principalExtension = if (isPrincipal) " extends PrincipalEntity" else ""

    // All IDs are newtypes, so always use .value to extract string
    val idToStringExpr = ".value"

    // Build parent set for Cedar entity
    val parentSet = entity.immediateParent match {
      case Some(parentName) =>
        val parentType = s"$namespace::$parentName"
        val parentIdField = parentName.take(1).toLowerCase + parentName.drop(1) + "Id"
        val quote = '"'
        s"Set(CedarEntityUid($quote$parentType$quote, a.$parentIdField.value))"
      case None =>
        "Set.empty"
    }

    // Build attribute map
    val attrMap = if (entity.attributes.nonEmpty) {
      val attrs = entity.attributes.map { attr =>
        val valueExpr = renderCedarValueExprWithPrefix("a.", attr, namespace, className)
        val quote = '"'
        s"$quote${attr.name}$quote -> $valueExpr"
      }
      s"Map(\n              ${attrs.mkString(",\n              ")}\n            )"
    } else {
      "Map.empty"
    }

    // Build getParentIds
    val parentExtraction = entity.immediateParent match {
      case Some(parentName) =>
        val parentType = s"$namespace::$parentName"
        val parentIdField = parentName.take(1).toLowerCase + parentName.drop(1) + "Id"
        val quote = '"'
        s"List($quote$parentType$quote -> a.$parentIdField.value)"
      case None =>
        "Nil"
    }

    // Generate nested case classes for record attributes
    val nestedClasses = entity.attributes
      .flatMap(_.nestedRecord)
      .map { nested =>
        renderNestedRecordClass(nested, namespace, className)
      }
      .mkString("\n")

    s"""${doc}    final case class $className(
$allFields
    )$principalExtension

    object $className {
$nestedClasses
      /** CedarEntityType instance for automatic conversion and type-safe registration */
      implicit val cedarEntityType: CedarEntityType.Aux[$className, $idType] = new CedarEntityType[$className] {
        type Id = $idType
        val entityType: String = "$fullEntityType"

        def toCedarEntity(a: $className): CedarEntity = CedarEntity(
          entityType = entityType,
          entityId = a.id$idToStringExpr,
          parents = $parentSet,
          attributes = $attrMap
        )

        def getParentIds(a: $className): List[(String, String)] = $parentExtraction
      }
    }"""
  }

  // ============================================================================
  // Unified DSL - Resource-centric
  // ============================================================================

  private def renderDsl(ir: CedarIR, pkg: String, dslName: String): String = {
    val header = fileHeader(pkg)
    val domains = SchemaToIR.buildDomains(ir)
    val namespace = ir.namespace

    // No additional type imports needed - ID types come from EntityIds
    val imports =
      s"""import cedar4s.auth.{AuthCheck, CedarSession, CedarAuthError, DeferredAuthCheck, FlatMap, CedarResource, Principal, CedarAction}
import cedar4s.entities.{EntityStore, ResourceRef, CedarEntity, CedarEntityType, CedarValue}
import cedar4s.schema.{CedarEntityUid, ShapeId, ShapeTag, Hints, CedarHints}
import cedar4s.{Bijection, Newtype}"""

    val resourceObjects = domains
      .map { domain =>
        renderDomainDslObject(domain, ir, dslName)
      }
      .mkString("\n")

    // Generate nested Actions object (content from renderActions)
    val actionsObject = renderActionsObjectContent(ir, namespace)

    // Generate nested Principal object (content from renderPrincipals)
    val principalObject = renderPrincipalObjectContent(ir, namespace)

    // Generate nested Resource object (content from renderResource)
    val resourceObject = renderResourceObjectContent(ir, pkg, namespace)

    // Generate nested Entity object (content from renderEntities)
    val entityObject = renderEntityObjectContent(ir, namespace)

    s"""$header
$imports

/**
  * Resource-centric DSL for $namespace authorization.
  *
  * All authorization checks use the `.on(id)` pattern which automatically
  * resolves the resource hierarchy via EntityStore:
  *
  * {{{
  * import $pkg.$dslName._
  *
  * implicit val entityStore: EntityStore[F] = ...
  * implicit val cedarSession: CedarSession[F] = ...
  *
  * // Instance actions - target the entity directly
  * Document.View.on(docId).require
  *
  * // Container actions - target the parent (e.g., create Document in Folder)
  * // The action name includes the child entity prefix
  * Folder.DocumentCreate.on(folderId).require
  * }}}
  *
  * Container actions (like create, list) are placed on the parent/container entity
  * as declared in the Cedar schema's `resource:` field.
  */
object $dslName {
$resourceObjects

$actionsObject

$principalObject

$resourceObject

$entityObject
}
"""
  }

  private def renderDomainDslObject(domain: DomainIR, ir: CedarIR, dslName: String): String = {
    // Note: domain.name is now the RESOURCE TYPE (entity that actions target)
    // Actions may have different domain prefixes (e.g., PullRequest::create on Repository)
    val resourceEntity = domain.entity
    val resourceTypeName = domain.name
    val escapedResourceType = escapeIdent(resourceTypeName)

    // Get the entity's ID type for .on(id)
    val idType = s"${resourceTypeName}Id"
    val domainType = s"PolicyDomain.$escapedResourceType.type"
    val ownershipType = ownershipPhantomType(resourceEntity.ownership, dslName)

    // Generate action wrapper objects
    val actions = domain.actions
      .map { action =>
        renderActionDslWrapper(action, resourceEntity, ir, dslName)
      }
      .mkString("\n")

    // Generate capability methods (only .on(id) based)
    val capabilityMethods = renderDomainCapabilityMethods(domain, ir, idType, escapedResourceType, dslName)

    s"""
  /**
    * DSL for $resourceTypeName resources.
    *
    * Actions targeting $resourceTypeName resources. Use `.on(id)` to create
    * authorization checks that automatically resolve the resource hierarchy.
    */
  object $escapedResourceType {
$actions
$capabilityMethods
  }"""
  }

  private def renderActionDslWrapper(
      action: ActionIR,
      resourceEntity: EntityIR,
      ir: CedarIR,
      dslName: String
  ): String = {
    val namespace = ir.namespace
    val resourceTypeName = resourceEntity.name
    val escapedResourceType = escapeIdent(resourceTypeName)

    // Use action.dslObjectName which handles container actions
    // - Instance action: Document::view -> "View"
    // - Container action: Document::create on Folder -> "DocumentCreate"
    val actionObjectName = escapeIdent(action.dslObjectName)

    // The action is defined in Actions grouped by its domain (action name prefix)
    val actionDomain = escapeIdent(action.domain)
    val actionPath = s"$dslName.Actions.$actionDomain.${escapeIdent(action.objectName)}"

    // Note: For container actions, actionType is from the action's domain, not resource type
    // e.g., PullRequest::create -> Actions.PullRequest.PullRequestAction
    val actionType = s"$dslName.Actions.$actionDomain.${action.domain}Action"

    // Resource type is the entity being targeted
    val domainType = s"PolicyDomain.$escapedResourceType.type"
    val ownershipType = ownershipPhantomType(resourceEntity.ownership, dslName)
    val resourceType = s"$dslName.Resource.Resource[$domainType, $ownershipType]"
    val requestType = s"AuthCheck.Single[Nothing, $actionType, $resourceType]"

    // ID type for this resource entity
    val idType = s"${resourceTypeName}Id"
    val entityType = s"$namespace::$resourceTypeName"

    // For root entities, still generate .on(id) - just no parents to resolve
    val docPrefix = if (action.isContainerAction) {
      s"Create/list ${action.domain} within this $resourceTypeName."
    } else {
      s"${action.objectName} action on $resourceTypeName."
    }

    s"""
    /**
      * $docPrefix
      *
      * Use `.on(id)` to create an authorization check:
      * {{{
      * $escapedResourceType.$actionObjectName.on(${resourceTypeName.toLowerCase}Id).require
      * }}}
      */
    object $actionObjectName {
      /**
        * Create a deferred authorization check that resolves via EntityStore.
        *
        * The EntityStore will fetch the $resourceTypeName entity and resolve
        * its parent hierarchy automatically.
        *
        * @param id The $resourceTypeName ID
        * @return A deferred check that can be executed with .run, .require, or .isAllowed
        */
      def on[F[_]](id: $idType)(
          implicit session: CedarSession[F],
          flatMap: FlatMap[F]
      ): DeferredAuthCheck[F, $idType, $actionType, $resourceType] =
        DeferredAuthCheck(
          entityType = "$entityType",
          entityId = id,
          actionValue = $actionPath,
          buildResource = (ref: ResourceRef) => $dslName.Resource.fromResourceRef[$domainType, $ownershipType](ref)
        )
    }"""
  }

  private def renderDomainCapabilityMethods(
      domain: DomainIR,
      ir: CedarIR,
      idType: String,
      escapedResourceType: String,
      dslName: String
  ): String = {
    val resourceTypeName = domain.name
    val namespace = ir.namespace
    val actionEntityType = s"$namespace::Action"
    val entityType = s"$namespace::$resourceTypeName"

    // Actions for this resource may come from different domains
    // e.g., Repository has PullRequest::create, Issue::create, Repository::view, etc.
    // We need to return the action names, not typed actions (since types differ)

    s"""
    /**
      * Get allowed action names for a specific $resourceTypeName.
      *
      * Returns the names of all actions the current principal can perform
      * on the specified $resourceTypeName resource.
      *
      * @param id The $resourceTypeName ID
      * @return F containing the set of allowed action names
      */
    def allowedActionNames[F[_]](id: $idType)(
        implicit session: CedarSession[F],
        flatMap: FlatMap[F]
    ): F[Set[String]] = {
      // First resolve the resource via EntityStore
      flatMap.flatMap(session.entityStore.loadEntityWithParents("$entityType", id.value)) {
        case Some((entity, parentIds)) =>
          val resource = $dslName.Resource.fromResourceRef[PolicyDomain.$escapedResourceType.type, ${ownershipPhantomType(
        domain.entity.ownership,
        dslName
      )}](
            ResourceRef("$entityType", Some(id.value), parentIds)
          )
          val allActions = Set(${domain.actions.map(a => s""""${a.fullName}"""").mkString(", ")})
          session.getAllowedActions(resource, "$actionEntityType", allActions)
        case None =>
          flatMap.pure(Set.empty[String])
      }
    }"""
  }
}
