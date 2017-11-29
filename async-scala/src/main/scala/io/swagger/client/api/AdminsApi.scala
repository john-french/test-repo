package io.swagger.client.api

import io.swagger.client.model.InventoryItem
import com.wordnik.swagger.client._
import scala.concurrent.Future
import collection.mutable

class AdminsApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  def addInventory(inventoryItem: Option[InventoryItem] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[InventoryItem]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/inventory"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]


    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(inventoryItem))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }


}
