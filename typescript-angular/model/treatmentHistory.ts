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

import { ResponseHistory } from './responseHistory';


export interface TreatmentHistory {
    /**
     * Name of the Domain through which the Treatment was made
     */
    domainName?: string;
    /**
     * Name of the Channel on which the Treatment was made
     */
    channelName?: string;
    /**
     * Identifier for a conversation (which groups together potentially many Requests and treatments)
     */
    conversation?: string;
    /**
     * Unique identifier for the request that resulted in this treatment
     */
    requestId?: number;
    /**
     * Time at which the Request was made
     */
    requestTimestamp?: string;
    /**
     * Unique identifier for this treatment
     */
    treatmentId?: number;
    /**
     * Time at which the Treatment was made
     */
    treatmentTimestamp?: string;
    /**
     * Identifier for the Action that was offered in the Treatment
     */
    actionId?: string;
    /**
     * Name of the Actionn
     */
    actionName?: string;
    /**
     * Collection of Responses to this Treatment.
     */
    responses?: Array<ResponseHistory>;
}
