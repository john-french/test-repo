package io.swagger.client.model

import org.joda.time.DateTime
import java.util.UUID


case class InventoryItem (
  id: UUID,
name: String,
releaseDate: String,
manufacturer: Manufacturer
)
