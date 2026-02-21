package cedar4s.entities

import cedar4s.schema.CedarEntityUid

/** Cedar entity representation.
  *
  * Entities in Cedar have:
  *   - A unique identifier (type + id)
  *   - Optional parent entities (for hierarchy)
  *   - Optional attributes
  *
  * The parent relationships are crucial for Cedar's `in` operator to work correctly for hierarchical authorization
  * checks like `resource in principal.customer`.
  */
final case class CedarEntity(
    entityType: String,
    entityId: String,
    parents: Set[CedarEntityUid] = Set.empty,
    attributes: Map[String, CedarValue] = Map.empty
) {

  /** Get the entity UID */
  def uid: CedarEntityUid = CedarEntityUid(entityType, entityId)
}

/** Cedar value types for entity attributes.
  *
  * These map to Cedar's type system:
  *   - String, Long, Bool: primitive types
  *   - Set: homogeneous collections
  *   - Record: structured data with named fields
  *   - Entity: reference to another entity
  *   - Extension types: ipaddr, decimal, datetime, duration
  */
sealed trait CedarValue

object CedarValue {
  import java.time.{Duration, Instant}

  // Primitive types
  final case class StringValue(value: String) extends CedarValue
  final case class LongValue(value: Long) extends CedarValue
  final case class BoolValue(value: Boolean) extends CedarValue

  // Composite types
  final case class SetValue(values: Set[CedarValue]) extends CedarValue
  final case class RecordValue(fields: Map[String, CedarValue]) extends CedarValue
  final case class EntityValue(uid: CedarEntityUid) extends CedarValue

  // Extension types
  /** IP address (IPv4 or IPv6) - stored as string representation */
  final case class IpAddrValue(value: String) extends CedarValue

  /** Decimal number with fixed precision */
  final case class DecimalValue(value: BigDecimal) extends CedarValue

  /** Date and time with timezone (Cedar 3.x+) */
  final case class DatetimeValue(value: Instant) extends CedarValue

  /** Duration/time interval (Cedar 3.x+) */
  final case class DurationValue(value: Duration) extends CedarValue

  /** Convenience constructors */
  def string(s: String): CedarValue = StringValue(s)
  def long(l: Long): CedarValue = LongValue(l)
  def bool(b: Boolean): CedarValue = BoolValue(b)
  def set(values: CedarValue*): CedarValue = SetValue(values.toSet)
  def set(values: Set[CedarValue]): CedarValue = SetValue(values)
  def record(fields: (String, CedarValue)*): CedarValue = RecordValue(fields.toMap)
  def entity(entityType: String, entityId: String): CedarValue =
    EntityValue(CedarEntityUid(entityType, entityId))
  def entity(uid: CedarEntityUid): CedarValue = EntityValue(uid)

  // Extension type constructors
  /** Create an IP address value */
  def ipaddr(ip: String): CedarValue = IpAddrValue(ip)

  /** Create a decimal value */
  def decimal(d: BigDecimal): CedarValue = DecimalValue(d)

  /** Create a decimal value from a string (e.g., "12.34") */
  def decimal(s: String): CedarValue = DecimalValue(BigDecimal(s))

  /** Create a datetime value */
  def datetime(instant: Instant): CedarValue = DatetimeValue(instant)

  /** Create a datetime value from epoch millis */
  def datetime(epochMillis: Long): CedarValue = DatetimeValue(Instant.ofEpochMilli(epochMillis))

  /** Create a duration value */
  def duration(d: Duration): CedarValue = DurationValue(d)

  /** Create a duration value from milliseconds */
  def durationMillis(millis: Long): CedarValue = DurationValue(Duration.ofMillis(millis))

  /** Create a set of string values */
  def stringSet(values: Set[String]): CedarValue =
    SetValue(values.map(StringValue.apply))

  /** Create a set of entity references (e.g., for permission sets like `admins: Set<User>`) */
  def entitySet(ids: Set[String], entityType: String): CedarValue =
    SetValue(ids.map(id => EntityValue(CedarEntityUid(entityType, id))))
}

/** Collection of Cedar entities for authorization.
  *
  * This represents the entity slice needed for a particular authorization request. It includes the principal, resource,
  * and all parent entities in the hierarchy.
  */
final case class CedarEntities(
    entities: Set[CedarEntity] = Set.empty
) {
  def +(entity: CedarEntity): CedarEntities = {
    // If an entity with the same UID already exists, keep the existing one
    if (entities.exists(_.uid == entity.uid)) this
    else CedarEntities(entities + entity)
  }
  def ++(other: CedarEntities): CedarEntities = {
    // Entities in `this` take precedence: skip any incoming entity whose UID is already present
    val existingUids = entities.map(_.uid)
    val incoming = other.entities.filterNot(e => existingUids.contains(e.uid))
    CedarEntities(entities ++ incoming)
  }
  def isEmpty: Boolean = entities.isEmpty
  def nonEmpty: Boolean = entities.nonEmpty
  def size: Int = entities.size

  /** Find an entity by its UID */
  def find(uid: CedarEntityUid): Option[CedarEntity] =
    entities.find(e => e.entityType == uid.entityType && e.entityId == uid.entityId)

  /** Get all entities of a specific type */
  def ofType(entityType: String): Set[CedarEntity] =
    entities.filter(_.entityType == entityType)
}

object CedarEntities {
  val empty: CedarEntities = CedarEntities()

  def apply(entities: CedarEntity*): CedarEntities = CedarEntities(entities.toSet)

  def fromSet(entities: Set[CedarEntity]): CedarEntities = CedarEntities(entities)
}
