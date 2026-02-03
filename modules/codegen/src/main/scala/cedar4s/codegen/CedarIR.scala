package cedar4s.codegen

import cedar4s.schema.{OwnershipType, ShapeId}

/** Intermediate Representation for Cedar code generation.
  *
  * The IR is a simplified, codegen-focused view of the Cedar schema. It captures exactly what's needed to generate
  * Scala code, with all complex schema analysis (hierarchy, ownership) already resolved.
  */

// ============================================================================
// Top-Level IR
// ============================================================================

/** Complete IR for a Cedar schema.
  *
  * @param namespace
  *   The Cedar namespace (e.g., "Robotsecurity")
  * @param entities
  *   All entity IRs with resolved ownership and parent chains
  * @param actions
  *   All action IRs grouped by domain
  */
final case class CedarIR(
    namespace: String,
    entities: List[EntityIR],
    actions: List[ActionIR],
    commonTypes: List[CommonTypeIR] = Nil
) {

  /** Get entities grouped by their domain (for action grouping) */
  def entitiesByName: Map[String, EntityIR] =
    entities.map(e => e.name -> e).toMap

  /** Get actions grouped by domain (action name prefix) - for reference only */
  def actionsByDomain: Map[String, List[ActionIR]] =
    actions.groupBy(_.domain)

  /** Get actions grouped by resource type (the actual target entity) */
  def actionsByResourceType: Map[String, List[ActionIR]] =
    actions.groupBy(_.resourceType)

  /** Get domains that have actions defined */
  def domainsWithActions: Set[String] =
    actions.map(_.domain).toSet

  /** Get resource types that have actions targeting them */
  def resourceTypesWithActions: Set[String] =
    actions.map(_.resourceType).toSet

  /** Get root entities (no parents) */
  def rootEntities: List[EntityIR] =
    entities.filter(_.ownership == OwnershipType.Root)

  /** Get all unique principal types referenced across all actions */
  def allPrincipalTypes: Set[String] =
    actions.flatMap(_.principalTypes).toSet

  /** Get principal -> actions mapping (which actions can this principal type perform?) */
  def actionsByPrincipal: Map[String, List[ActionIR]] =
    actions
      .flatMap(a => a.principalTypes.map(_ -> a))
      .groupBy(_._1)
      .map { case (k, v) => k -> v.map(_._2) }

  /** Common types by name */
  def commonTypesByName: Map[String, CommonTypeIR] =
    commonTypes.map(t => t.name -> t).toMap
}

// ============================================================================
// Entity IR
// ============================================================================

/** IR for a single Cedar entity type.
  *
  * @param name
  *   Entity name (e.g., "Customer", "Mission")
  * @param ownership
  *   Ownership classification (Root, Direct, Indirect)
  * @param parentChain
  *   Ordered list of parent entity names from root to immediate parent
  * @param attributes
  *   Entity attributes (for potential data class generation)
  * @param enumValues
  *   For enum entities, the allowed EID values (e.g., ["draft", "published", "archived"])
  * @param doc
  *   Documentation string
  */
final case class EntityIR(
    name: String,
    ownership: OwnershipType,
    parentChain: List[String],
    attributes: List[AttributeIR] = Nil,
    enumValues: Option[List[String]] = None,
    doc: Option[String] = None
) {

  /** ShapeId for this entity */
  def shapeId(namespace: String): ShapeId = ShapeId(namespace, name)

  /** Is this a root entity? */
  def isRoot: Boolean = ownership == OwnershipType.Root

  /** Is this an enum entity? */
  def isEnum: Boolean = enumValues.isDefined

  /** Depth in hierarchy (0 for root) */
  def depth: Int = parentChain.length

  /** Immediate parent, if any */
  def immediateParent: Option[String] = parentChain.lastOption

  /** Parameter name for this entity's ID in factory methods. E.g., "Customer" -> "customerId", "Mission" -> "missionId"
    */
  def idParamName: String = name.take(1).toLowerCase + name.drop(1) + "Id"

  /** Newtype name for this entity's ID. E.g., "Customer" -> "CustomerId"
    */
  def idTypeName: String = name + "Id"

  /** Scala-safe type name (escapes keywords) */
  def scalaName: String = ScalaKeywords.escape(name)
}

// ============================================================================
// Action IR
// ============================================================================

/** IR for a single Cedar action.
  *
  * Important distinction between `domain` and `resourceType`:
  *   - `domain`: The entity name prefix from the action name (e.g., "PullRequest" from "PullRequest::create")
  *   - `resourceType`: The actual entity type that is the resource for this action
  *
  * For instance actions (view, edit, delete), these are typically the same: action "Document::view" appliesTo {
  * resource: Document } -> domain=Document, resourceType=Document
  *
  * For container actions (create, list), these differ: action "Document::create" appliesTo { resource: Folder } ->
  * domain=Document, resourceType=Folder
  *
  * The DSL groups actions by `resourceType` so container actions appear on the container entity.
  *
  * @param name
  *   Simple action name (e.g., "view", "create")
  * @param domain
  *   Domain from action name prefix (e.g., "Document" from "Document::view")
  * @param fullName
  *   Full action name as in schema (e.g., "Document::view")
  * @param isCollectionAction
  *   True if this operates on parent (create/list) - domain != resourceType
  * @param resourceType
  *   The entity type that is the resource (determines DSL placement)
  * @param principalTypes
  *   Entity types that can be the principal for this action
  * @param contextAttributes
  *   Context attributes for this action (from schema's context type)
  * @param doc
  *   Documentation string
  */
final case class ActionIR(
    name: String,
    domain: String,
    fullName: String,
    isCollectionAction: Boolean,
    resourceType: String,
    principalTypes: List[String] = Nil,
    contextAttributes: List[AttributeIR] = Nil,
    doc: Option[String] = None
) {

  /** ShapeId for this action */
  def shapeId(namespace: String): ShapeId = ShapeId(namespace, fullName)

  /** Scala object name (PascalCase) */
  def objectName: String = name.split("[-_]").map(_.capitalize).mkString

  /** Full DSL object name including domain prefix when it differs from resource type.
    *
    * Examples:
    *   - Document::view on Document -> "View"
    *   - PullRequest::create on Repository -> "PullRequestCreate"
    */
  def dslObjectName: String =
    if (isContainerAction) s"$domain$objectName"
    else objectName

  /** True if this is a container action (domain != resourceType) */
  def isContainerAction: Boolean = domain != resourceType

  /** Cedar action reference string */
  def cedarActionRef(namespace: String): String =
    s"""$namespace::Action::"$fullName""""
}

// ============================================================================
// Attribute IR
// ============================================================================

/** IR for an entity attribute.
  *
  * @param name
  *   Attribute name
  * @param scalaType
  *   Scala type string (e.g., "String", "Long", "Option[String]")
  * @param cedarType
  *   Cedar type string (e.g., "String", "Long", "Bool")
  * @param optional
  *   Whether this attribute is optional
  * @param nestedRecord
  *   If this is a record type, the nested record IR
  * @param doc
  *   Documentation string
  */
final case class AttributeIR(
    name: String,
    scalaType: String,
    cedarType: String,
    optional: Boolean = false,
    nestedRecord: Option[NestedRecordIR] = None,
    doc: Option[String] = None,
    resolvedType: cedar4s.schema.SchemaType
) {

  /** Scala field name (camelCase, escaped if keyword).
    *
    * Preserves existing camelCase in the first segment while handling hyphen/underscore
    * separators. E.g.:
    *   - "firstName" -> "firstName" (preserved)
    *   - "first_name" -> "firstName" (converted to camelCase)
    *   - "first-name" -> "firstName" (converted to camelCase)
    *   - "FirstName" -> "firstName" (lowercased first char only)
    */
  def fieldName: String = {
    val camel = name
      .split("[-_]")
      .zipWithIndex
      .map { case (part, i) =>
        if (i == 0) {
          // For the first part, only lowercase the first character to preserve camelCase
          // e.g., "firstName" stays "firstName", "FirstName" becomes "firstName"
          if (part.isEmpty) part
          else part.head.toLower + part.tail
        } else {
          // For subsequent parts, capitalize the first character
          part.capitalize
        }
      }
      .mkString
    if (ScalaKeywords.contains(camel)) s"`$camel`" else camel
  }

  /** Whether this attribute has a nested record type */
  def hasNestedRecord: Boolean = nestedRecord.isDefined
}

// ============================================================================
// Nested Record IR
// ============================================================================

/** IR for a nested record type (inline record in an attribute).
  *
  * When an entity has an attribute like:
  * {{{
  * entity Location = {
  *   address: { street: String, city: String }
  * };
  * }}}
  *
  * The `address` attribute will have a NestedRecordIR that generates a nested case class like `Location.Address`.
  *
  * @param className
  *   The generated class name (e.g., "Address")
  * @param attributes
  *   The attributes of the nested record
  */
final case class NestedRecordIR(
    className: String,
    attributes: List[AttributeIR]
)

// ============================================================================
// Common Type IR
// ============================================================================

/** IR for a common type (type alias) declaration.
  *
  * @param name
  *   Common type name
  * @param typeExpr
  *   Original schema type expression
  * @param resolvedType
  *   Resolved schema type with type refs expanded
  * @param doc
  *   Documentation string
  */
final case class CommonTypeIR(
    name: String,
    typeExpr: cedar4s.schema.SchemaType,
    resolvedType: cedar4s.schema.SchemaType,
    attributes: List[AttributeIR] = Nil,
    elementRecord: Option[NestedRecordIR] = None,
    doc: Option[String] = None
) {
  def scalaName: String = ScalaKeywords.escape(name)
}

// ============================================================================
// Domain IR (for grouping)
// ============================================================================

/** IR for a domain (group of related actions).
  *
  * @param name
  *   Domain name (e.g., "Mission")
  * @param entity
  *   The entity this domain corresponds to
  * @param actions
  *   Actions in this domain
  */
final case class DomainIR(
    name: String,
    entity: EntityIR,
    actions: List[ActionIR]
)

// ============================================================================
// Utilities
// ============================================================================

/** Scala reserved keywords that need escaping */
object ScalaKeywords {
  val keywords: Set[String] = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  def contains(s: String): Boolean = keywords.contains(s)

  def escape(name: String): String = if (contains(name)) s"`$name`" else name
}
