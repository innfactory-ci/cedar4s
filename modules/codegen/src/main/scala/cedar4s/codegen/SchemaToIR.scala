package cedar4s.codegen

import cedar4s.schema._

/** Transforms a parsed Cedar schema into the codegen IR.
  *
  * This is the bridge between the generic schema parser and the Scala code generator. It resolves:
  *   - Entity hierarchy and ownership classification
  *   - Parent chains for each entity
  *   - Action grouping by domain
  */
object SchemaToIR {

  /** Configuration for schema-to-IR transformation.
    *
    * @param tenantRoots
    *   Override set of root entity names (empty = auto-detect)
    */
  final case class Config(
      tenantRoots: Set[String] = Set.empty
  )

  object Config {
    val default: Config = Config()
  }

  /** Transform a Cedar schema to IR.
    *
    * @param schema
    *   The parsed Cedar schema
    * @param config
    *   Transformation configuration
    * @return
    *   The codegen IR
    */
  def transform(schema: CedarSchema, config: Config = Config.default): CedarIR = {
    val namespace = schema.namespaces.headOption
      .flatMap(_.name)
      .map(_.value)
      .getOrElse("Cedar")

    val hierarchy = schema.entityHierarchy
    val commonTypesByName = schema.allCommonTypes.map(ct => ct.name -> ct).toMap

    // Transform entities
    val entities = schema.allEntities.map { entity =>
      transformEntity(entity, hierarchy, namespace, commonTypesByName)
    }

    // Transform actions
    val actions = schema.allActions.flatMap { action =>
      transformAction(action, namespace, commonTypesByName)
    }

    val commonTypes = schema.allCommonTypes.map { ct =>
      val resolved = resolveType(ct.typeExpr, commonTypesByName)
      val attributes = ct.typeExpr match {
        case SchemaType.Record(record) =>
          record.attributes.map(attr => transformAttribute(attr, commonTypesByName))
        case _ => Nil
      }
      val elementRecord = ct.typeExpr match {
        case SchemaType.SetOf(SchemaType.Record(record)) =>
          val className = toPascalCase(ct.name) + "Item"
          val nestedAttrs = record.attributes.map(attr => transformAttribute(attr, commonTypesByName))
          Some(NestedRecordIR(className, nestedAttrs))
        case _ => None
      }
      CommonTypeIR(
        name = ct.name,
        typeExpr = ct.typeExpr,
        resolvedType = resolved,
        attributes = attributes,
        elementRecord = elementRecord,
        doc = ct.doc
      )
    }

    CedarIR(
      namespace = namespace,
      entities = entities,
      actions = actions,
      commonTypes = commonTypes
    )
  }

  /** Transform a single entity declaration to IR.
    */
  private def transformEntity(
      entity: EntityDecl,
      hierarchy: EntityHierarchy,
      namespace: String,
      commonTypesByName: Map[String, CommonTypeDecl]
  ): EntityIR = {
    val name = entity.name

    // Get parent chain first (needed for ownership determination)
    val parentChain = getParentChain(name, hierarchy)

    // Determine ownership based on parent chain length
    val ownership = determineOwnership(name, parentChain)

    // Transform attributes
    val attributes = entity.attributes.map(attr => transformAttribute(attr, commonTypesByName))

    EntityIR(
      name = name,
      ownership = ownership,
      parentChain = parentChain,
      attributes = attributes,
      enumValues = entity.enumValues,
      doc = entity.doc
    )
  }

  /** Determine ownership classification for an entity.
    *
    * Uses the parent chain (not direct parents) to determine ownership:
    *   - No parents = Root
    *   - One parent in chain (direct child of root) = Direct
    *   - Two or more parents in chain = Indirect
    */
  private def determineOwnership(entityName: String, parentChain: List[String]): OwnershipType = {
    parentChain.length match {
      case 0 => OwnershipType.Root
      case 1 => OwnershipType.Direct
      case _ => OwnershipType.Indirect
    }
  }

  /** Get the parent chain from root to immediate parent.
    *
    * When an entity has multiple parents (like Mission in [Location, Customer]), we prefer the path through
    * intermediate entities (Location) rather than the direct path to root (Customer). This matches the logical
    * hierarchy where Mission → Location → Customer.
    */
  private def getParentChain(entityName: String, hierarchy: EntityHierarchy): List[String] = {
    val roots = hierarchy.roots
    if (roots.contains(entityName)) {
      Nil
    } else {
      // Get all paths to any root
      val allPaths = roots.toList.flatMap { root =>
        hierarchy.pathsTo(entityName, root)
      }

      if (allPaths.isEmpty) Nil
      else {
        // Prefer paths that go through non-root intermediate entities
        // This ensures Mission → Location → Customer rather than Mission → Customer
        val pathsWithIntermediates = allPaths.filter(_.length > 2)
        val selectedPath = if (pathsWithIntermediates.nonEmpty) {
          // Among paths with intermediates, pick the one that goes through
          // entities at greater depths (more specific hierarchy)
          pathsWithIntermediates.maxBy { path =>
            path.map(e => hierarchy.depthOf(e)).sum
          }
        } else {
          // Fall back to shortest path if no intermediate paths exist
          allPaths.minBy(_.length)
        }

        // Drop the entity itself, reverse to get root-first order
        selectedPath.drop(1).reverse
      }
    }
  }

  /** Transform a single action declaration to IR.
    *
    * Returns None if the action doesn't have a domain prefix.
    */
  private def transformAction(
      action: ActionDecl,
      namespace: String,
      commonTypesByName: Map[String, CommonTypeDecl]
  ): Option[ActionIR] = {
    // Parse domain from action name (e.g., "Mission::create" -> "Mission")
    action.domain.map { domain =>
      val simpleName = action.actionName

      // Get the resource type from schema
      val resourceType = action.resourceTypes.headOption
        .map(_.simple)
        .getOrElse(domain)

      // Container action = domain differs from resource type
      // e.g., "Document::create" with resource: Folder -> domain=Document, resourceType=Folder
      val isCollection = domain != resourceType

      // Get principal types
      val principalTypes = action.principalTypes.map(_.simple)

      // Get context attributes
      val contextAttributes = action.contextType
        .map(_.attributes.map(attr => transformAttribute(attr, commonTypesByName)))
        .getOrElse(Nil)

      ActionIR(
        name = simpleName,
        domain = domain,
        fullName = action.name,
        isCollectionAction = isCollection,
        resourceType = resourceType,
        principalTypes = principalTypes,
        contextAttributes = contextAttributes,
        doc = action.doc
      )
    }
  }

  /** Transform an attribute declaration to IR.
    */
  private def transformAttribute(attr: AttributeDecl, commonTypesByName: Map[String, CommonTypeDecl]): AttributeIR = {
    val resolved = resolveType(attr.typeExpr, commonTypesByName)
    val (scalaType, cedarType, nestedRecord) = schemaTypeToScala(attr.typeExpr, attr.name, commonTypesByName)

    val finalScalaType = if (attr.optional) s"Option[$scalaType]" else scalaType

    AttributeIR(
      name = attr.name,
      scalaType = finalScalaType,
      cedarType = cedarType,
      optional = attr.optional,
      nestedRecord = nestedRecord,
      doc = attr.doc,
      resolvedType = resolved
    )
  }

  /** Convert a Cedar schema type to Scala type string.
    *
    * @param attrName
    *   The attribute name, used to generate class names for nested records
    * @return
    *   (scalaType, cedarType, nestedRecord) tuple
    */
  private def schemaTypeToScala(
      schemaType: SchemaType,
      attrName: String = "",
      commonTypesByName: Map[String, CommonTypeDecl]
  ): (String, String, Option[NestedRecordIR]) = schemaType match {
    case SchemaType.Primitive(PrimitiveType.String) => ("String", "String", None)
    case SchemaType.Primitive(PrimitiveType.Long)   => ("Long", "Long", None)
    case SchemaType.Primitive(PrimitiveType.Bool)   => ("Boolean", "Bool", None)
    // Extension types - use wrapper types that can serialize to Cedar properly
    case SchemaType.Extension(ExtensionType.ipaddr)   => ("String", "ipaddr", None)
    case SchemaType.Extension(ExtensionType.decimal)  => ("BigDecimal", "decimal", None)
    case SchemaType.Extension(ExtensionType.datetime) => ("java.time.Instant", "datetime", None)
    case SchemaType.Extension(ExtensionType.duration) => ("java.time.Duration", "duration", None)
    case SchemaType.EntityRef(name)                   => (s"String", name.simple, None) // Entity refs are IDs
    case SchemaType.SetOf(elem)                       =>
      val nestedRecord = elem match {
        case SchemaType.Record(record) =>
          val className = toPascalCase(attrName) + "Item"
          val nestedAttrs = record.attributes.map(attr => transformAttribute(attr, commonTypesByName))
          Some(NestedRecordIR(className, nestedAttrs))
        case _ => None
      }
      val (elemScala, elemCedar, _) = schemaTypeToScala(elem, attrName, commonTypesByName)
      val elemType = nestedRecord.map(_.className).getOrElse(elemScala)
      (s"Set[$elemType]", s"Set<$elemCedar>", nestedRecord)
    case SchemaType.Record(record) =>
      // Generate a nested case class for this record
      val className = toPascalCase(attrName)
      val nestedAttrs = record.attributes.map(attr => transformAttribute(attr, commonTypesByName))
      val nestedIR = NestedRecordIR(className, nestedAttrs)
      (className, "Record", Some(nestedIR))
    case SchemaType.TypeRef(name) => (ScalaKeywords.escape(name.simple), cedarTypeString(schemaType), None)
  }

  private def resolveType(
      schemaType: SchemaType,
      commonTypesByName: Map[String, CommonTypeDecl],
      seen: Set[String] = Set.empty
  ): SchemaType = schemaType match {
    case SchemaType.TypeRef(name) =>
      val key = name.simple
      if (seen.contains(key)) schemaType
      else {
        commonTypesByName.get(key) match {
          case Some(common) =>
            resolveType(common.typeExpr, commonTypesByName, seen + key)
          case None => schemaType
        }
      }
    case SchemaType.SetOf(element) =>
      SchemaType.SetOf(resolveType(element, commonTypesByName, seen))
    case SchemaType.Record(record) =>
      val resolvedAttrs = record.attributes.map { attr =>
        val resolvedAttrType = resolveType(attr.typeExpr, commonTypesByName, seen)
        attr.copy(typeExpr = resolvedAttrType)
      }
      SchemaType.Record(RecordType(resolvedAttrs))
    case other => other
  }

  private def cedarTypeString(schemaType: SchemaType): String = schemaType match {
    case SchemaType.Primitive(PrimitiveType.String)   => "String"
    case SchemaType.Primitive(PrimitiveType.Long)     => "Long"
    case SchemaType.Primitive(PrimitiveType.Bool)     => "Bool"
    case SchemaType.Extension(ExtensionType.ipaddr)   => "ipaddr"
    case SchemaType.Extension(ExtensionType.decimal)  => "decimal"
    case SchemaType.Extension(ExtensionType.datetime) => "datetime"
    case SchemaType.Extension(ExtensionType.duration) => "duration"
    case SchemaType.EntityRef(name)                   => name.value
    case SchemaType.SetOf(element)                    => s"Set<${cedarTypeString(element)}>"
    case SchemaType.Record(_)                         => "Record"
    case SchemaType.TypeRef(name)                     => name.value
  }

  /** Convert a name to PascalCase for class names. "address" -> "Address", "home_address" -> "HomeAddress"
    */
  private def toPascalCase(name: String): String = {
    name.split("[-_]").map(_.capitalize).mkString
  }

  /** Build domain IRs by grouping entities with their actions.
    *
    * Actions are grouped by `resourceType` (the actual target entity), not by `domain` (the action name prefix). This
    * means container actions like "Document::create" with `resource: Folder` will appear on the Folder domain, not
    * Document.
    *
    * Use this for the main DSL where actions are placed on their target resource.
    */
  def buildDomains(ir: CedarIR): List[DomainIR] = {
    val entitiesByName = ir.entitiesByName
    val actionsByResourceType = ir.actionsByResourceType

    // Create domains for entities that are resources for actions
    ir.resourceTypesWithActions.toList.sorted.flatMap { resourceType =>
      entitiesByName.get(resourceType).map { entity =>
        DomainIR(
          name = resourceType,
          entity = entity,
          actions = actionsByResourceType.getOrElse(resourceType, Nil)
        )
      }
    }
  }

  /** Build domain IRs grouped by action domain (action name prefix).
    *
    * This groups actions by their name prefix (e.g., "PullRequest" from "PullRequest::create"), regardless of what
    * resource type they target. Use this for the Actions object where action case objects are organized by their domain
    * prefix.
    */
  def buildDomainsByActionDomain(ir: CedarIR): List[DomainIR] = {
    val entitiesByName = ir.entitiesByName
    val actionsByDomain = ir.actionsByDomain

    // Create domains for entities that have actions named after them
    ir.domainsWithActions.toList.sorted.flatMap { domainName =>
      entitiesByName.get(domainName).map { entity =>
        DomainIR(
          name = domainName,
          entity = entity,
          actions = actionsByDomain.getOrElse(domainName, Nil)
        )
      }
    }
  }
}
