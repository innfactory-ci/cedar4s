package cedar4s.codegen

import munit.FunSuite
import cedar4s.schema.{CedarSchema, OwnershipType}

/** Tests for Cedar code generation.
  */
class CedarCodegenSpec extends FunSuite {

  val testSchema = """
    namespace Robotsecurity {
      entity Customer = {
        "name": String,
      };
      
      entity Location in [Customer] = {
        "name": String,
        "address"?: String,
      };
      
      entity Mission in [Location] = {
        "name": String,
        "status": String,
      };
      
      entity User;
      
      action "Customer::read" appliesTo {
        principal: [User],
        resource: [Customer],
      };
      
      action "Customer::update" appliesTo {
        principal: [User],
        resource: [Customer],
      };
      
      action "Location::read" appliesTo {
        principal: [User],
        resource: [Location],
      };
      
      action "Location::create" appliesTo {
        principal: [User],
        resource: [Customer],
      };
      
      action "Mission::read" appliesTo {
        principal: [User],
        resource: [Mission],
      };
      
      action "Mission::create" appliesTo {
        principal: [User],
        resource: [Location],
      };
    }
  """

  // ===========================================================================
  // Schema to IR Tests
  // ===========================================================================

  test("SchemaToIR transforms entities with correct ownership") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    assert(result.isRight)

    // Parse schema and check IR directly
    val schema = cedar4s.schema.CedarSchema.parseUnsafe(testSchema)
    val ir = SchemaToIR.transform(schema)

    val customer = ir.entities.find(_.name == "Customer").get
    assertEquals(customer.ownership, OwnershipType.Root)
    assert(customer.parentChain.isEmpty)

    val location = ir.entities.find(_.name == "Location").get
    assertEquals(location.ownership, OwnershipType.Direct)
    assertEquals(location.parentChain, List("Customer"))

    val mission = ir.entities.find(_.name == "Mission").get
    assertEquals(mission.ownership, OwnershipType.Indirect)
    assertEquals(mission.parentChain, List("Customer", "Location"))
  }

  test("SchemaToIR transforms actions with correct domain grouping") {
    val schema = cedar4s.schema.CedarSchema.parseUnsafe(testSchema)
    val ir = SchemaToIR.transform(schema)

    assert(ir.actionsByDomain.keys.toSet.contains("Customer"))
    assert(ir.actionsByDomain.keys.toSet.contains("Location"))
    assert(ir.actionsByDomain.keys.toSet.contains("Mission"))

    val customerActions = ir.actionsByDomain("Customer")
    assert(customerActions.map(_.name).contains("read"))
    assert(customerActions.map(_.name).contains("update"))

    val missionActions = ir.actionsByDomain("Mission")
    assert(missionActions.map(_.name).contains("read"))
    assert(missionActions.map(_.name).contains("create"))

    // Check collection action detection
    val missionCreate = missionActions.find(_.name == "create").get
    assert(missionCreate.isCollectionAction)

    val missionRead = missionActions.find(_.name == "read").get
    assert(!missionRead.isCollectionAction)
  }

  // NOTE: SchemaToIR refinement configuration tests have been removed.
  // The newtype-based system auto-generates ${EntityName}Id newtypes for ALL entities.
  // No configuration or @refinement annotations are needed anymore.

  // ===========================================================================
  // Scala Renderer Tests
  // ===========================================================================

  test("ScalaRenderer generates PolicyDomain hierarchy") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    val files = result.toOption.get

    val policyDomain = files("PolicyDomain.scala")
    assert(policyDomain.contains("sealed trait PolicyDomain"))
    assert(policyDomain.contains("case object Customer extends PolicyDomain"))
    assert(policyDomain.contains("case object Location extends PolicyDomain"))
    assert(policyDomain.contains("case object Mission extends PolicyDomain"))
    assert(policyDomain.contains("OwnershipType.Root"))
    assert(policyDomain.contains("OwnershipType.Direct"))
    assert(policyDomain.contains("OwnershipType.Indirect"))
  }

  test("ScalaRenderer generates Actions object with domain grouping") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    val files = result.toOption.get

    // Actions are now nested in the unified DSL file
    val dsl = files("Robotsecurity.scala")
    assert(dsl.contains("object Actions"))
    assert(dsl.contains("sealed trait Action"))
    assert(dsl.contains("object Customer"))
    assert(dsl.contains("object Location"))
    assert(dsl.contains("object Mission"))
    assert(dsl.contains("case object Read extends"))
    assert(dsl.contains("case object Create extends"))
    assert(dsl.contains("val isCollectionAction = true"))
    assert(dsl.contains("val isCollectionAction = false"))
    assert(dsl.contains("""Robotsecurity::Action::"Mission::"""))
  }

  test("ScalaRenderer generates Resource with factory methods using newtypes") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    val files = result.toOption.get

    // Resource is now nested in the unified DSL file
    val dsl = files("Robotsecurity.scala")
    assert(dsl.contains("object Resource"))
    assert(dsl.contains("final case class Resource"))
  }

  // NOTE: Refinement tests have been removed.
  // The newtype-based system auto-generates ${EntityName}Id newtypes for ALL entities.
  // No configuration or @refinement annotations are needed anymore.
  // See the "EntityIds.scala content" test below for newtype generation tests.

  test("ScalaRenderer generates common types and uses them in entities") {
    val schema = """
      namespace Test {
        type Email = String;
        type Address = {
          "street": String,
          "owner": User,
        };
        
        entity User;
        entity Account = {
          "email": Email,
          "address": Address,
        };
      }
    """

    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val commonTypes = files("CommonTypes.scala")
    assert(commonTypes.contains("type Email = String"))
    assert(commonTypes.contains("final case class Address"))

    // Entities are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Entity"))
    assert(dsl.contains("email: Email"))
    assert(dsl.contains("address: Address"))
  }

  test("ScalaRenderer converts nested entity references in records") {
    val schema = """
      namespace Test {
        entity User;
        entity Team = {
          "metadata": {
            "owner": User,
            "admins": Set<User>,
          },
        };
      }
    """

    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    assert(result.isRight, s"Schema parse failed: ${result.left.getOrElse("")}")
    val files = result.toOption.get

    // Entities are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Entity"))
    assert(dsl.contains("""CedarValue.entity("Test::User", owner)"""))
    assert(dsl.contains("""CedarValue.entitySet(admins, "Test::User")"""))
  }

  test("ScalaRenderer escapes Scala keywords in entity names") {
    val schemaJson =
      """
        |{
        |  "Test": {
        |    "entityTypes": {
        |      "type": {}
        |    }
        |  }
        |}
        |""".stripMargin

    val result = CedarCodegen.generateFromString(schemaJson, "test.cedar")
    val files = result.toOption.get

    val policyDomain = files("PolicyDomain.scala")
    assert(policyDomain.contains("case object `type` extends PolicyDomain"))

    // Entities are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Entity"))
    assert(dsl.contains("final case class `type`"))
  }

  test("ScalaRenderer generates package declaration") {
    val result = CedarCodegen.generateFromString(testSchema, "com.example.auth.cedar")
    val files = result.toOption.get

    files.values.foreach { content =>
      assert(content.contains("package com.example.auth.cedar"))
    }
  }

  test("ScalaRenderer generates auto-generated header") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    val files = result.toOption.get

    files.values.foreach { content =>
      assert(content.contains("AUTO-GENERATED - DO NOT EDIT"))
    }
  }

  // ===========================================================================
  // CedarCodegen Integration Tests
  // ===========================================================================

  test("CedarCodegen generates all expected files") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    val files = result.toOption.get

    assert(files.keys.toSet.contains("PolicyDomain.scala"))
    assert(files.keys.toSet.contains("EntityIds.scala"))
    assert(files.keys.toSet.contains("CommonTypes.scala"))
    // Actions, Resource, Entities, and Principal are now nested in the unified DSL file
    assert(files.keys.toSet.contains("Robotsecurity.scala"))
  }

  test("CedarCodegen generates EntityIds.scala with newtypes for all entities") {
    val result = CedarCodegen.generateFromString(testSchema, "test.cedar")
    val files = result.toOption.get

    val entityIds = files("EntityIds.scala")

    // Should import Newtype and Bijection
    assert(entityIds.contains("import cedar4s.{Bijection, Newtype}"))

    // Should generate newtypes for each entity
    assert(entityIds.contains("object CustomerId extends Newtype[String]"))
    assert(entityIds.contains("type CustomerId = CustomerId.Type"))

    assert(entityIds.contains("object LocationId extends Newtype[String]"))
    assert(entityIds.contains("type LocationId = LocationId.Type"))

    assert(entityIds.contains("object MissionId extends Newtype[String]"))
    assert(entityIds.contains("type MissionId = MissionId.Type"))

    assert(entityIds.contains("object UserId extends Newtype[String]"))
    assert(entityIds.contains("type UserId = UserId.Type"))

    // Should have docstrings
    assert(entityIds.contains("/** ID type for Customer entities */"))
  }

  test("CedarCodegen handles empty schema gracefully") {
    val emptySchema = "namespace Empty {}"
    val result = CedarCodegen.generateFromString(emptySchema, "test.cedar")
    assert(result.isRight)
  }

  test("CedarCodegen handles schema with only entities (no actions)") {
    val entitiesOnly = """
      namespace Test {
        entity Customer;
        entity Location in [Customer];
      }
    """
    val result = CedarCodegen.generateFromString(entitiesOnly, "test.cedar")
    assert(result.isRight)

    val files = result.toOption.get
    // Actions are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Actions"))
    assert(dsl.contains("val all: Set[Action] = Set()"))
  }

  // ===========================================================================
  // Edge Cases
  // ===========================================================================

  test("Edge case: handles action names with hyphens") {
    val schema = """
      namespace Test {
        entity Document;
        entity User;
        action "Document::view-all" appliesTo {
          principal: [User],
          resource: [Document],
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    // Actions are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Actions"))
    assert(dsl.contains("case object ViewAll")) // Converted to PascalCase
  }

  test("Edge case: handles deeply nested hierarchy with newtypes") {
    val schema = """
      namespace Test {
        entity A;
        entity B in [A];
        entity C in [B];
        entity D in [C];
        entity User;

        action "D::read" appliesTo {
          principal: [User],
          resource: [D],
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    // Resource is now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Resource"))

    // Also verify EntityIds.scala has the newtypes
    val entityIds = files("EntityIds.scala")
    assert(entityIds.contains("object AId extends Newtype[String]"))
    assert(entityIds.contains("object BId extends Newtype[String]"))
    assert(entityIds.contains("object CId extends Newtype[String]"))
    assert(entityIds.contains("object DId extends Newtype[String]"))
  }

  test("Edge case: handles nested record attributes") {
    val schema = """
      namespace Test {
        entity Location = {
          "name": String,
          "address": { "street": String, "city": String, "zip": Long },
        };
        entity User;
        action "Location::read" appliesTo {
          principal: [User],
          resource: [Location],
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    // Entities are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Entity"))
    // Should generate a nested case class for the address record
    assert(dsl.contains("final case class Address"))
    assert(dsl.contains("street: String"))
    assert(dsl.contains("city: String"))
    assert(dsl.contains("zip: Long"))
    // The Location entity should use the nested type
    assert(dsl.contains("address: Location.Address"))
    // The nested class should have toCedarValue method
    assert(dsl.contains("def toCedarValue: CedarValue"))
  }

  test("Edge case: handles optional nested record attributes") {
    val schema = """
      namespace Test {
        entity Location = {
          "name": String,
          "address"?: { "street": String, "city": String },
        };
        entity User;
        action "Location::read" appliesTo {
          principal: [User],
          resource: [Location],
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    // Entities are now nested in the unified DSL file
    val dsl = files("Test.scala")
    assert(dsl.contains("object Entity"))
    // Should generate a nested case class for the address record
    assert(dsl.contains("final case class Address"))
    // The Location entity should use Option of the nested type
    assert(dsl.contains("address: Option[Location.Address]"))
  }

  // ===========================================================================
  // Smithy Integration (Optional)
  // ===========================================================================

  test("Smithy integration is disabled by default (smithy = None)") {
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    // Create temp schema file
    val tempFile = Files.createTempFile("test-schema", ".cedarschema")
    Files.write(tempFile, testSchema.getBytes(StandardCharsets.UTF_8))

    try {
      val args = CedarCodegenArgs(
        input = CedarInput(schemaFiles = List(tempFile)),
        scala = ScalaOutput(
          outputDir = Paths.get("/tmp"),
          packageName = "test.cedar"
        ),
        smithy = None // Disabled
      )

      val result = CedarCodegen.run(args).toOption.get

      // Should not generate any .smithy files
      assert(result.smithyFiles.isEmpty)

      // Should still generate Scala files (now consolidated in unified DSL file)
      assert(result.scalaFiles.keys.map(_.toString).toSet.contains("Robotsecurity.scala"))
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  test("Smithy integration generates Smithy files when smithy = Some(...)") {
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    // Create temp schema file
    val tempFile = Files.createTempFile("test-schema", ".cedarschema")
    Files.write(tempFile, testSchema.getBytes(StandardCharsets.UTF_8))

    try {
      val args = CedarCodegenArgs(
        input = CedarInput(schemaFiles = List(tempFile)),
        scala = ScalaOutput(
          outputDir = Paths.get("/tmp"),
          packageName = "test.cedar"
        ),
        smithy = Some(
          SmithyOutput(
            outputDir = Paths.get("/tmp/smithy"),
            namespace = "com.example.api.authz"
          )
        )
      )

      val result = CedarCodegen.run(args).toOption.get

      // Should generate action enums for each domain
      assert(result.smithyFiles.keys.map(_.toString).toSet.contains("CustomerAction.smithy"))
      assert(result.smithyFiles.keys.map(_.toString).toSet.contains("LocationAction.smithy"))
      assert(result.smithyFiles.keys.map(_.toString).toSet.contains("MissionAction.smithy"))

      // Check Customer actions
      val customerSmithy = result.smithyFiles.values.find(_.contains("CustomerAction")).get
      assert(customerSmithy.contains("enum CustomerAction"))
      assert(customerSmithy.contains("READ = \"read\""))
      assert(customerSmithy.contains("UPDATE = \"update\""))
      assert(customerSmithy.contains("list CustomerAllowedActions"))
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  test("Smithy enum names use UPPER_CASE") {
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    val schema = """
      namespace Test {
        entity Document;
        entity User;
        action "Document::read-file" appliesTo {
          principal: [User],
          resource: [Document],
        };
        action "Document::create_draft" appliesTo {
          principal: [User],
          resource: [Document],
        };
      }
    """

    // Create temp schema file
    val tempFile = Files.createTempFile("test-schema", ".cedarschema")
    Files.write(tempFile, schema.getBytes(StandardCharsets.UTF_8))

    try {
      val args = CedarCodegenArgs(
        input = CedarInput(schemaFiles = List(tempFile)),
        scala = ScalaOutput(
          outputDir = Paths.get("/tmp"),
          packageName = "test.cedar"
        ),
        smithy = Some(
          SmithyOutput(
            outputDir = Paths.get("/tmp/smithy"),
            namespace = "com.example.api"
          )
        )
      )

      val result = CedarCodegen.run(args).toOption.get

      val documentSmithy = result.smithyFiles.values.find(_.contains("DocumentAction")).get
      assert(documentSmithy.contains("READ_FILE = \"read-file\""))
      assert(documentSmithy.contains("CREATE_DRAFT = \"create_draft\""))
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  // ===========================================================================
  // Bug Fix Tests - Attribute Name Case Preservation
  // ===========================================================================

  test("Bug fix: preserves camelCase attribute names like firstName") {
    val schema = """
      namespace Test {
        entity User = {
          "firstName": String,
          "lastName": String,
          "emailAddress": String,
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // Should preserve camelCase - firstName should NOT become firstname
    assert(dsl.contains("firstName: String"), "firstName should be preserved as camelCase")
    assert(dsl.contains("lastName: String"), "lastName should be preserved as camelCase")
    assert(dsl.contains("emailAddress: String"), "emailAddress should be preserved as camelCase")
    
    // Should NOT contain the broken lowercase versions
    assert(!dsl.contains("firstname:"), "Should not lowercase entire first part")
    assert(!dsl.contains("lastname:"), "Should not lowercase entire first part")
  }

  test("Bug fix: converts snake_case to camelCase correctly") {
    val schema = """
      namespace Test {
        entity User = {
          "first_name": String,
          "last_name": String,
          "email_address": String,
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // Should convert snake_case to camelCase
    assert(dsl.contains("firstName: String"), "first_name should become firstName")
    assert(dsl.contains("lastName: String"), "last_name should become lastName")
    assert(dsl.contains("emailAddress: String"), "email_address should become emailAddress")
  }

  test("Bug fix: converts hyphen-case to camelCase correctly") {
    val schema = """
      namespace Test {
        entity User = {
          "first-name": String,
          "last-name": String,
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // Should convert hyphen-case to camelCase
    assert(dsl.contains("firstName: String"), "first-name should become firstName")
    assert(dsl.contains("lastName: String"), "last-name should become lastName")
  }

  test("Bug fix: lowercases PascalCase first char but preserves rest") {
    val schema = """
      namespace Test {
        entity User = {
          "FirstName": String,
          "LastName": String,
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // Should lowercase first char only: FirstName -> firstName
    assert(dsl.contains("firstName: String"), "FirstName should become firstName")
    assert(dsl.contains("lastName: String"), "LastName should become lastName")
  }

  // ===========================================================================
  // Bug Fix Tests - Enum Entity Code Generation
  // ===========================================================================

  test("Bug fix: generates Scala enum for Cedar enum entities") {
    val schema = """
      namespace Test {
        entity Status enum ["draft", "published", "archived"];
        entity User;
        action "User::read" appliesTo {
          principal: [User],
          resource: [User],
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // Should generate an enum with the values
    assert(dsl.contains("Status"), "Should contain Status type")
    // The enum should have the values defined
    assert(dsl.contains("draft") || dsl.contains("Draft"), "Should contain draft value")
    assert(dsl.contains("published") || dsl.contains("Published"), "Should contain published value")
    assert(dsl.contains("archived") || dsl.contains("Archived"), "Should contain archived value")
  }

  // ===========================================================================
  // Bug Fix Tests - TypeRef in Sets Resolution
  // ===========================================================================

  test("Bug fix: resolves TypeRef in Set to entity reference correctly") {
    val schema = """
      namespace Test {
        type RoleRef = Role;
        entity Role;
        entity User = {
          "roles": Set<RoleRef>,
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // When TypeRef resolves to an entity, should use entitySet not stringSet
    // The generated code should reference the entity properly
    assert(dsl.contains("roles:"), "Should have roles field")
    // The resolved type should generate entity serialization, not string conversion
    // Check that we're using entitySet for the entity reference
    assert(dsl.contains(""""Test::Role"""") || dsl.contains("entitySet"), 
           "TypeRef to entity should use entity serialization with entity type")
  }

  test("Bug fix: resolves nested TypeRef in Sets") {
    val schema = """
      namespace Test {
        type TagList = Set<String>;
        entity Document = {
          "tags": TagList,
        };
        entity User;
        action "Document::read" appliesTo {
          principal: [User],
          resource: [Document],
        };
      }
    """
    val result = CedarCodegen.generateFromString(schema, "test.cedar")
    val files = result.toOption.get

    val dsl = files("Test.scala")
    // Should handle TypeRef that resolves to Set<String>
    assert(dsl.contains("tags:"), "Should have tags field")
    // The resolved type should generate string set serialization
    assert(dsl.contains("stringSet") || dsl.contains("CedarValue.string"), 
           "TypeRef to Set<String> should use string serialization")
  }
}
