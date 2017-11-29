package io.swagger.client.api

import io.swagger.client.model.InventoryItem
import com.wordnik.swagger.client._
import scala.concurrent.Future
import collection.mutable

class DevelopersApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  def searchInventory(searchString: Option[String] = None,
      skip: Option[Integer] = None,
      limit: Option[Integer] = None
      )(implicit reader: ClientResponseReader[List[InventoryItem]]): Future[List[InventoryItem]] = {
    // create path and map variables
    val path = (addFmt("/inventory"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    searchString match {
    case Some(param) => queryParams += "searchString" -> param.toString
    case _ => queryParams
    }
    skip match {
    case Some(param) => queryParams += "skip" -> param.toString
    case _ => queryParams
    }
    limit match {
    case Some(param) => queryParams += "limit" -> param.toString
    case _ => queryParams
    }

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }


}
