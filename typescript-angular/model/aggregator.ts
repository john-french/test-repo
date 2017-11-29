/**
 * Best Next Action API
 * APIs for Best Next Action capabilities.   Note that the error specification used by this API is based  on the IETF draft: https://tools.ietf.org/html/rfc7807 
 *
 * OpenAPI spec version: v1
 * 
 *
 * NOTE: This class is auto generated by the swagger code generator program.
 * https://github.com/swagger-api/swagger-codegen.git
 * Do not edit the class manually.
 */



/**
 * Definition of an aggregation operator
 */
export interface Aggregator {
    /**
     * System name of the aggregator
     */
    name?: string;
    /**
     * The name to be displayed to the user
     */
    displayName?: string;
    /**
     * Name of the datatype that is returned by the aggregator
     */
    datatype?: string;
}
