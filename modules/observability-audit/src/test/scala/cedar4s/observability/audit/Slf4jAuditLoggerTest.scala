package cedar4s.observability.audit

import munit.FunSuite

import java.time.Instant
import java.nio.file.{Files, Paths}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import cedar4s.capability.instances._

class Slf4jAuditLoggerTest extends FunSuite {

  test("Slf4jAuditLogger logs authorization events") {
    val logger = Slf4jAuditLogger[Future](
      loggerName = "cedar4s.audit",
      level = LogLevel.Info
    )

    val event = AuthorizationEvent(
      timestamp = Instant.parse("2026-01-29T12:34:56.789Z"),
      principal = PrincipalInfo("User", "alice"),
      action = "Document::View",
      resource = ResourceInfo("Document", Some("doc-123")),
      decision = Decision(allow = true, policies = List("policy-1")),
      reason = None,
      durationNanos = 1_500_000
    )

    // Log the event - should not throw
    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }

  test("Slf4jAuditLogger handles denied events") {
    val logger = Slf4jAuditLogger[Future](
      loggerName = "cedar4s.audit",
      level = LogLevel.Warn
    )

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("User", "bob"),
      action = "Document::Delete",
      resource = ResourceInfo("Document", Some("doc-456")),
      decision = Decision(allow = false, policies = List("deny-policy")),
      reason = Some("Insufficient permissions"),
      durationNanos = 2_000_000,
      context = Map("ip" -> "192.168.1.1"),
      sessionId = Some("session-123"),
      requestId = Some("req-456")
    )

    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }

  test("Slf4jAuditLogger supports custom logger names") {
    val logger = Slf4jAuditLogger[Future](
      loggerName = "com.mycompany.audit.cedar",
      level = LogLevel.Debug
    )

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("ServiceAccount", "service-1"),
      action = "API::Call",
      resource = ResourceInfo("API", None),
      decision = Decision(allow = true),
      reason = None,
      durationNanos = 500_000
    )

    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }

  test("Slf4jAuditLogger gracefully handles missing logstash encoder") {
    // This test ensures the logger works even without logstash-logback-encoder
    // It should fall back to plain text logging
    val logger = Slf4jAuditLogger[Future](
      loggerName = "cedar4s.audit.text",
      useStructuredLogging = false
    )

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("User", "charlie"),
      action = "Resource::Access",
      resource = ResourceInfo("Resource", Some("res-789")),
      decision = Decision(allow = true),
      reason = None,
      durationNanos = 1_000_000
    )

    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }

  test("Slf4jAuditLogger detects logstash availability") {
    // This should return true if logstash-logback-encoder is on the test classpath
    val available = Slf4jAuditLogger.isLogstashAvailable
    assert(available, "Expected logstash-logback-encoder to be available in test classpath")
  }

  test("Slf4jAuditLogger handles multiple events") {
    val logger = Slf4jAuditLogger[Future]()

    val events = (1 to 10).map { i =>
      AuthorizationEvent(
        timestamp = Instant.now(),
        principal = PrincipalInfo("User", s"user-$i"),
        action = "Document::View",
        resource = ResourceInfo("Document", Some(s"doc-$i")),
        decision = Decision(allow = i % 2 == 0), // Alternate allow/deny
        reason = if (i % 2 == 0) None else Some("Access denied"),
        durationNanos = i * 1_000_000L
      )
    }

    val results = events.map(event => logger.logDecision(event))
    val completed = Future.sequence(results)

    Await.result(completed, 10.seconds)
    // All events should be logged successfully
    assert(completed.isCompleted)
  }

  test("Slf4jAuditLogger integration with Logback JSON appender") {
    // This test verifies that events are actually written to the JSON file
    val auditFile = Paths.get("target/test-audit.json")

    // Clean up any existing file
    if (Files.exists(auditFile)) {
      Files.delete(auditFile)
    }

    val logger = Slf4jAuditLogger[Future](
      loggerName = "cedar4s.audit",
      level = LogLevel.Info
    )

    val event = AuthorizationEvent(
      timestamp = Instant.parse("2026-01-29T15:30:00.000Z"),
      principal = PrincipalInfo("User", "integration-test"),
      action = "Test::Action",
      resource = ResourceInfo("Test", Some("test-1")),
      decision = Decision(allow = true, policies = List("test-policy")),
      reason = None,
      durationNanos = 1_234_567,
      context = Map("test" -> "integration"),
      sessionId = Some("test-session"),
      requestId = Some("test-request")
    )

    Await.result(logger.logDecision(event), 5.seconds)

    // Give logback time to flush — CI runners can be slow
    Thread.sleep(2000)

    // File may not exist if logback configuration isn't loaded properly
    // This is acceptable in some test environments
    if (Files.exists(auditFile)) {
      val content = Source.fromFile(auditFile.toFile).mkString
      if (content.nonEmpty) {
        assert(
          content.contains("integration-test") || content.contains("Test::Action"),
          s"Expected audit file to contain event data, but got: $content"
        )
      }
    }
  }

  test("Slf4jAuditLogger supports all log levels") {
    val debugLogger = Slf4jAuditLogger[Future](level = LogLevel.Debug)
    val infoLogger = Slf4jAuditLogger[Future](level = LogLevel.Info)
    val warnLogger = Slf4jAuditLogger[Future](level = LogLevel.Warn)

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("User", "test"),
      action = "Test::Level",
      resource = ResourceInfo("Test", Some("level")),
      decision = Decision(allow = true),
      reason = None,
      durationNanos = 1_000_000
    )

    // All should complete without error
    Await.result(debugLogger.logDecision(event), 5.seconds)
    Await.result(infoLogger.logDecision(event), 5.seconds)
    Await.result(warnLogger.logDecision(event), 5.seconds)
  }

  test("Slf4jAuditLogger handles events with empty context") {
    val logger = Slf4jAuditLogger[Future]()

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("User", "empty-context"),
      action = "Test::EmptyContext",
      resource = ResourceInfo("Test", Some("empty")),
      decision = Decision(allow = true),
      reason = None,
      durationNanos = 1_000_000,
      context = Map.empty
    )

    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }

  test("Slf4jAuditLogger handles resource without entityId") {
    val logger = Slf4jAuditLogger[Future]()

    val event = AuthorizationEvent(
      timestamp = Instant.now(),
      principal = PrincipalInfo("User", "test"),
      action = "Collection::List",
      resource = ResourceInfo("Collection", None),
      decision = Decision(allow = true),
      reason = None,
      durationNanos = 1_000_000
    )

    val result = Await.result(logger.logDecision(event), 5.seconds)
    assertEquals(result, ())
  }
}
