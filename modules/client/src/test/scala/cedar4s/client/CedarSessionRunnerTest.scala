package cedar4s.client

import cedar4s.auth._
import cedar4s.capability.Monad
import cedar4s.capability.instances.{futureSync, futureMonadError}
import cedar4s.entities._
import cedar4s.schema.CedarEntityUid
import munit.FunSuite

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/** Integration tests for CedarSessionRunner with the full authorization flow.
  *
  * These tests verify:
  *   - CedarSessionRunner properly integrates with CedarEngine and EntityStore
  *   - AuthCheck DSL works correctly with the runner
  *   - AND/OR composition evaluates correctly
  *   - Conditional checks (.when) work properly
  *   - Batch operations work correctly
  *   - Default principal is used when not specified
  *   - Explicit principal overrides default
  */
class CedarSessionRunnerTest extends FunSuite {

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  // ===========================================================================
  // Test Domain Types - Simulates generated code
  // ===========================================================================

  // Principals
  case class TestUser(id: String) extends Principal {
    val entityType = "Test::User"
    val entityId = id
  }

  case class TestServiceAccount(id: String) extends Principal {
    val entityType = "Test::ServiceAccount"
    val entityId = id
  }

  // Actions
  sealed trait TestAction extends CedarAction

  case class ReadAction(resourceType: String, resourceId: Option[String]) extends TestAction {
    val name = "read"
    val cedarAction = s"Test::Action::\"read\""
    val isCollectionAction = false
  }

  case class WriteAction(resourceType: String, resourceId: Option[String]) extends TestAction {
    val name = "write"
    val cedarAction = s"Test::Action::\"write\""
    val isCollectionAction = false
  }

  case class DeleteAction(resourceType: String, resourceId: Option[String]) extends TestAction {
    val name = "delete"
    val cedarAction = s"Test::Action::\"delete\""
    val isCollectionAction = false
  }

  // Resources
  case class DocumentResource(folderId: String, docId: String) extends CedarResource {
    val entityType = "Test::Document"
    val entityId = Some(docId)
    val parents = List(("Test::Folder", folderId))
    def toCedarEntity: String = s"Test::Document::\"$docId\""
  }

  // DSL helpers (simulates generated ActionDsl)
  object Document {
    def Read(folderId: String, docId: String): AuthCheck.Single[Nothing, ReadAction, DocumentResource] =
      AuthCheck.single(
        ReadAction("Test::Document", Some(docId)),
        DocumentResource(folderId, docId)
      )

    def Write(folderId: String, docId: String): AuthCheck.Single[Nothing, WriteAction, DocumentResource] =
      AuthCheck.single(
        WriteAction("Test::Document", Some(docId)),
        DocumentResource(folderId, docId)
      )

    def Delete(folderId: String, docId: String): AuthCheck.Single[Nothing, DeleteAction, DocumentResource] =
      AuthCheck.single(
        DeleteAction("Test::Document", Some(docId)),
        DocumentResource(folderId, docId)
      )
  }

  // Principal evidence for tests
  implicit val canPerformUserRead: Principal.CanPerform[TestUser, ReadAction] = Principal.CanPerform.allow
  implicit val canPerformUserWrite: Principal.CanPerform[TestUser, WriteAction] = Principal.CanPerform.allow
  implicit val canPerformUserDelete: Principal.CanPerform[TestUser, DeleteAction] = Principal.CanPerform.allow
  implicit val canPerformServiceRead: Principal.CanPerform[TestServiceAccount, ReadAction] = Principal.CanPerform.allow

  // ===========================================================================
  // Test Fixtures
  // ===========================================================================

  val engine: CedarEngine[Future] = CedarEngine.fromResources(
    policiesPath = "test-policies",
    policyFiles = Seq("ownership.cedar")
  )

  // Simple in-memory entity store for testing
  class TestEntityStore(entities: Map[CedarEntityUid, CedarEntity]) extends EntityStore[Future] {
    override def loadForRequest(principal: CedarPrincipal, resource: ResourceRef): Future[CedarEntities] = {
      val resourceEntities = resource.uid.toList.flatMap(uid => entities.get(uid))
      val parentEntities = resource.parentUids.flatMap(uid => entities.get(uid))
      Future.successful(principal.entities ++ CedarEntities.fromSet((resourceEntities ++ parentEntities).toSet))
    }

    override def loadForBatch(principal: CedarPrincipal, resources: Seq[ResourceRef]): Future[CedarEntities] = {
      val allUids = resources.flatMap(r => r.uid.toList ++ r.parentUids).toSet
      val loaded = allUids.flatMap(uid => entities.get(uid))
      Future.successful(principal.entities ++ CedarEntities.fromSet(loaded))
    }

    override def loadEntity(entityType: String, entityId: String): Future[Option[CedarEntity]] =
      Future.successful(entities.get(CedarEntityUid(entityType, entityId)))

    override def loadEntities(uids: Set[CedarEntityUid]): Future[CedarEntities] =
      Future.successful(CedarEntities.fromSet(uids.flatMap(entities.get)))

    override def loadEntityWithParents(
        entityType: String,
        entityId: String
    ): Future[Option[(CedarEntity, List[(String, String)])]] =
      Future.successful(entities.get(CedarEntityUid(entityType, entityId)).map(e => (e, Nil)))
  }

  // Test principal entity
  case class TestUserEntity(id: String)

  // Provide CedarEntityType for test principal
  implicit val testUserEntityType: CedarEntityType.Aux[TestUserEntity, String] =
    new CedarEntityType[TestUserEntity] {
      type Id = String
      val entityType: String = "Test::User"
      def toCedarEntity(a: TestUserEntity): CedarEntity = CedarEntity(
        entityType = entityType,
        entityId = a.id,
        parents = Set.empty,
        attributes = Map.empty
      )
      def getParentIds(a: TestUserEntity): List[(String, String)] = Nil
    }

  // Session helper that builds principals from TestUser/TestServiceAccount
  def sessionFor(user: TestUser, store: EntityStore[Future]): CedarSession[Future] = {
    val resolver = CedarRuntime.resolverFrom[Future, TestUserEntity] { principal =>
      Future.successful(Some(TestUserEntity(principal.entityId)))
    }
    CedarRuntime[Future, TestUserEntity](engine, store, resolver).session(user)
  }

  def await[A](f: Future[A]): A = Await.result(f, 5.seconds)

  implicit class FutureAwait[A](private val f: Future[A]) {
    def await: A = Await.result(f, 5.seconds)
  }

  // ===========================================================================
  // Basic Authorization Tests
  // ===========================================================================

  test("run returns Right for authorized request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val result = await(Document.Read("folder-1", "doc-1").run)
    assert(result.isRight, s"Owner should be authorized: $result")
  }

  test("run returns Left for unauthorized request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::User", "bob") -> CedarEntity("Test::User", "bob"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val result = await(Document.Read("folder-1", "doc-1").run)
    assert(result.isLeft, "Non-owner should be denied")
    assert(result.left.toOption.exists(_.message.contains("Permission denied")))
  }

  test("require succeeds for authorized request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    // Should not throw
    Document.Read("folder-1", "doc-1").require.await
  }

  test("require fails with error for unauthorized request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::User", "bob") -> CedarEntity("Test::User", "bob"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val ex = intercept[CedarAuthError.Unauthorized] {
      Document.Read("folder-1", "doc-1").require.await
    }
    assert(ex.message.contains("Permission denied"))
  }

  test("isAllowed returns true for authorized request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val allowed = await(Document.Read("folder-1", "doc-1").isAllowed)
    assert(allowed)
  }

  test("isAllowed returns false for unauthorized request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::User", "bob") -> CedarEntity("Test::User", "bob"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val allowed = await(Document.Read("folder-1", "doc-1").isAllowed)
    assert(!allowed)
  }

  // ===========================================================================
  // Explicit Principal Tests
  // ===========================================================================

  test("asPrincipal overrides default principal") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::User", "bob") -> CedarEntity("Test::User", "bob"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    // Default principal is alice
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    // Without explicit principal (alice) - should fail
    val aliceResult = await(Document.Read("folder-1", "doc-1").run)
    assert(aliceResult.isLeft, "Alice should not have access")

    // With explicit principal (bob) - should succeed
    val bobResult = await(Document.Read("folder-1", "doc-1").asPrincipal(TestUser("bob")).run)
    assert(bobResult.isRight, s"Bob (owner) should have access: $bobResult")
  }

  // ===========================================================================
  // Conditional Check Tests (.when)
  // ===========================================================================

  test("when condition skips check when false") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::User", "bob") -> CedarEntity("Test::User", "bob"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    // Without condition - should fail (alice is not owner)
    val normalResult = await(Document.Read("folder-1", "doc-1").run)
    assert(normalResult.isLeft, "Should fail without condition")

    // With false condition - should succeed (skipped)
    val skippedResult = await(Document.Read("folder-1", "doc-1").when(false).run)
    assert(skippedResult.isRight, "Should succeed when condition is false (skipped)")

    // With true condition - should fail (not skipped)
    val notSkippedResult = await(Document.Read("folder-1", "doc-1").when(true).run)
    assert(notSkippedResult.isLeft, "Should fail when condition is true (not skipped)")
  }

  // ===========================================================================
  // Composition Tests (& and |)
  // ===========================================================================

  test("AND composition fails if any request fails") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    // Alice owns doc-1 but not doc-2
    val check1 = Document.Read("folder-1", "doc-1") // Should pass
    val check2 = Document.Read("folder-1", "doc-2") // Should fail

    // AND should fail because doc-2 fails
    val andResult = await((check1 & check2).run)
    assert(andResult.isLeft, "AND should fail if any request fails")

    // Verify individual results
    assert(await(check1.run).isRight, "doc-1 alone should pass")
    assert(await(check2.run).isLeft, "doc-2 alone should fail")
  }

  test("AND composition passes if all requests pass") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    // Alice owns both documents
    val check1 = Document.Read("folder-1", "doc-1")
    val check2 = Document.Read("folder-1", "doc-2")

    val andResult = await((check1 & check2).run)
    assert(andResult.isRight, "AND should pass if all requests pass")
  }

  test("OR composition passes if any request passes") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val check1 = Document.Read("folder-1", "doc-1") // Should pass
    val check2 = Document.Read("folder-1", "doc-2") // Should fail

    // OR should pass because doc-1 passes
    val orResult = await((check1 | check2).run)
    assert(orResult.isRight, "OR should pass if any request passes")
  }

  test("OR composition fails if all requests fail") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "charlie"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val check1 = Document.Read("folder-1", "doc-1") // Should fail
    val check2 = Document.Read("folder-1", "doc-2") // Should fail

    val orResult = await((check1 | check2).run)
    assert(orResult.isLeft, "OR should fail if all requests fail")
    assert(orResult.left.toOption.exists(_.message.contains("None granted")))
  }

  // ===========================================================================
  // Batch Operation Tests
  // ===========================================================================

  test("batchRun returns correct results for each request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    val runner = sessionFor(TestUser("alice"), store)

    val requests = Seq(
      Document.Read("folder-1", "doc-1"), // Should pass (alice owns)
      Document.Read("folder-1", "doc-2") // Should fail (bob owns)
    )

    val results = await(runner.batchRun(requests))
    assertEquals(results.size, 2)
    assert(results(0).isRight, "First request should pass")
    assert(results(1).isLeft, "Second request should fail")
  }

  test("batchIsAllowed returns correct booleans for each request") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    val runner = sessionFor(TestUser("alice"), store)

    val requests = Seq(
      Document.Read("folder-1", "doc-1"),
      Document.Read("folder-1", "doc-2")
    )

    val results = await(runner.batchIsAllowed(requests))
    assertEquals(results, Seq(true, false))
  }

  test("filterAllowed returns only authorized items") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Document", "doc-2") -> CedarEntity(
        "Test::Document",
        "doc-2",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Document", "doc-3") -> CedarEntity(
        "Test::Document",
        "doc-3",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "alice"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    val runner = sessionFor(TestUser("alice"), store)

    val documents = Seq("doc-1", "doc-2", "doc-3")
    val allowed = await(runner.filterAllowed(documents)(docId => Document.Read("folder-1", docId)))

    // Alice owns doc-1 and doc-3, not doc-2
    assertEquals(allowed, Seq("doc-1", "doc-3"))
  }

  // ===========================================================================
  // Entity UID Collision Tests
  // ===========================================================================

  // Resolver that enriches the principal with role=admin
  val adminUserEntityType: CedarEntityType.Aux[TestUserEntity, String] =
    new CedarEntityType[TestUserEntity] {
      type Id = String
      val entityType: String = "Test::User"
      def toCedarEntity(a: TestUserEntity): CedarEntity = CedarEntity(
        entityType = entityType,
        entityId = a.id,
        parents = Set.empty,
        attributes = Map("role" -> CedarValue.string("admin"))
      )
      def getParentIds(a: TestUserEntity): List[(String, String)] = Nil
    }

  def adminSessionFor(user: TestUser, store: EntityStore[Future]): CedarSession[Future] = {
    val resolver = CedarRuntime.resolverFrom[Future, TestUserEntity] { principal =>
      Future.successful(Some(TestUserEntity(principal.entityId)))
    }
    implicit val et: CedarEntityType.Aux[TestUserEntity, String] = adminUserEntityType
    CedarRuntime[Future, TestUserEntity](engine, store, resolver).session(user)
  }

  // Store that injects a weaker (no role) copy of the principal entity alongside the requested resource.
  // This simulates a store returning an entity with the same UID as the principal but fewer attributes.
  def storeWithPrincipalUidCollision(docId: String): EntityStore[Future] = new EntityStore[Future] {
    override def loadForRequest(principal: CedarPrincipal, resource: ResourceRef): Future[CedarEntities] = {
      val collidingEntity = CedarEntity("Test::User", "alice") // same UID as principal, no role
      val docEntity = CedarEntity(
        "Test::Document",
        docId,
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      )
      val folderEntity = CedarEntity("Test::Folder", "folder-1")
      val resourceEntities = CedarEntities.fromSet(Set(collidingEntity, docEntity, folderEntity))
      Future.successful(principal.entities ++ resourceEntities)
    }
    override def loadForBatch(principal: CedarPrincipal, resources: Seq[ResourceRef]): Future[CedarEntities] =
      Future.successful(principal.entities)
    override def loadEntity(entityType: String, entityId: String): Future[Option[CedarEntity]] =
      Future.successful(None)
    override def loadEntities(uids: Set[CedarEntityUid]): Future[CedarEntities] =
        Future.successful(CedarEntities.empty)
    override def loadEntityWithParents(
        entityType: String,
        entityId: String
    ): Future[Option[(CedarEntity, List[(String, String)])]] =
        Future.successful(None)
  }

  test("principal entity wins when store returns same UID with fewer attributes") {
    // alice is resolved with role=admin; the store also returns Test::User::"alice" without role.
    // The principal version must win so the admin permit fires.
    implicit val runner: CedarSession[Future] =
      adminSessionFor(TestUser("alice"), storeWithPrincipalUidCollision("doc-collision"))

    val result = await(Document.Read("folder-1", "doc-collision").run)
    assert(result.isRight, s"Principal with role=admin should be authorized: $result")
  }

  test("non-admin is still denied when there is no UID collision") {
    // Sanity check: the same document, same store shape, but no role on the principal.
    implicit val runner: CedarSession[Future] =
      sessionFor(TestUser("alice"), storeWithPrincipalUidCollision("doc-no-admin"))

    val result = await(Document.Read("folder-1", "doc-no-admin").run)
    assert(result.isLeft, "Principal without role=admin should be denied")
  }

  // ===========================================================================
  // Error Handling Tests
  // ===========================================================================

  test("error includes action and resource information") {
    val entities = Map(
      CedarEntityUid("Test::User", "alice") -> CedarEntity("Test::User", "alice"),
      CedarEntityUid("Test::Document", "doc-1") -> CedarEntity(
        "Test::Document",
        "doc-1",
        attributes = Map("owner" -> CedarValue.entity("Test::User", "bob"))
      ),
      CedarEntityUid("Test::Folder", "folder-1") -> CedarEntity("Test::Folder", "folder-1")
    )

    val store = new TestEntityStore(entities)
    implicit val runner: CedarSession[Future] = sessionFor(TestUser("alice"), store)

    val result = await(Document.Write("folder-1", "doc-1").run)
    assert(result.isLeft)
    val error = result.left.toOption.get
    assert(error.message.contains("doc-1"), "Error should include resource ID")
    assert(error.message.contains("write"), "Error should include action")
  }
}
