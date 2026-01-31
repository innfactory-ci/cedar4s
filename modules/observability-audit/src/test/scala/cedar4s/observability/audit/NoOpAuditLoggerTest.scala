package cedar4s.observability.audit

import munit.FunSuite

import java.time.Instant
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cedar4s.capability.instances._

class NoOpAuditLoggerTest extends FunSuite {

  test("NoOpAuditLogger does nothing") {
    val logger = NoOpAuditLogger[Future]()

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("User", "alice"),
      action = "Document::View",
      resource = ResourceInfo("Document", Some("doc-123")),
      decision = Decision(allow = true),
      reason = None,
      durationNanos = 1_000_000
    )

    // Should complete without error
    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }

  test("NoOpAuditLogger handles multiple events") {
    val logger = NoOpAuditLogger[Future]()

    val events = (1 to 100).map { i =>
      AuthorizationEvent(
        timestamp = Instant.now(),
        principal = PrincipalInfo("User", s"user-$i"),
        action = "Document::View",
        resource = ResourceInfo("Document", Some(s"doc-$i")),
        decision = Decision(allow = true),
        reason = None,
        durationNanos = 1_000_000 * i
      )
    }

    // Should complete very quickly
    val start = System.nanoTime()
    events.foreach { event =>
      Await.result(logger.logDecision(event), 5.seconds)
    }
    val duration = (System.nanoTime() - start) / 1_000_000.0

    // NoOp should be fast (< 100ms for 100 events, generous for CI runners)
    assert(duration < 100.0, s"NoOp took too long: ${duration}ms")
  }
}
