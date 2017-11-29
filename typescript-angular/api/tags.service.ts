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

/* tslint:disable:no-unused-variable member-ordering */

import { Inject, Injectable, Optional }                      from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams }               from '@angular/common/http';

import { Observable }                                        from 'rxjs/Observable';
import '../rxjs-operators';

import { ErrorResponse } from '../model/errorResponse';
import { NewEntityResponse } from '../model/newEntityResponse';
import { Tag } from '../model/tag';
import { TagBase } from '../model/tagBase';
import { TagsResponse } from '../model/tagsResponse';

import { BASE_PATH, COLLECTION_FORMATS }                     from '../variables';
import { Configuration }                                     from '../configuration';
import { CustomHttpUrlEncodingCodec }                        from '../encoder';


@Injectable()
export class TagsService {

    protected basePath = 'https://virtserver.swaggerhub.com/PitneyBowes-CES/Best_Next_Actions/v1';
    public defaultHeaders = new HttpHeaders();
    public configuration = new Configuration();

    constructor(protected httpClient: HttpClient, @Optional()@Inject(BASE_PATH) basePath: string, @Optional() configuration: Configuration) {
        if (basePath) {
            this.basePath = basePath;
        }
        if (configuration) {
            this.configuration = configuration;
            this.basePath = basePath || configuration.basePath || this.basePath;
        }
    }

    /**
     * @param consumes string[] mime-types
     * @return true: consumes contains 'multipart/form-data', false: otherwise
     */
    private canConsumeForm(consumes: string[]): boolean {
        const form = 'multipart/form-data';
        for (let consume of consumes) {
            if (form === consume) {
                return true;
            }
        }
        return false;
    }


    /**
     * Retrieve Tags
     * Retrieves all available Tags, optionally filtered by Tag Type and Domain. Use the offset and limit parameters to retrieve subsets of the available Tags. 
     * @param tenantId Id of the tenant being accessed
     * @param tagType Type of Tags to return. 
     * @param domainId ID of the Domain whose Tags should be returned.  
     * @param query Return only those items whose name, description or tags (if available) contain the search string.
     * @param offset Number of items to skip before returning results. Default is 0.
     * @param limit Maximum number of items to return. 0 means return all items. Default is 20.
     */
    public tagsGet(tenantId: string, tagType?: string, domainId?: string, query?: string, offset?: number, limit?: number): Observable<TagsResponse> {
        if (tenantId === null || tenantId === undefined) {
            throw new Error('Required parameter tenantId was null or undefined when calling tagsGet.');
        }

        let queryParameters = new HttpParams({encoder: new CustomHttpUrlEncodingCodec()});
        if (tagType !== undefined) {
            queryParameters = queryParameters.set('tagType', <any>tagType);
        }
        if (domainId !== undefined) {
            queryParameters = queryParameters.set('domainId', <any>domainId);
        }
        if (query !== undefined) {
            queryParameters = queryParameters.set('query', <any>query);
        }
        if (offset !== undefined) {
            queryParameters = queryParameters.set('offset', <any>offset);
        }
        if (limit !== undefined) {
            queryParameters = queryParameters.set('limit', <any>limit);
        }

        let headers = this.defaultHeaders;
        if (tenantId !== undefined && tenantId !== null) {
            headers = headers.set('TenantId', String(tenantId));
        }

        // to determine the Accept header
        let httpHeaderAccepts: string[] = [
            'application/json'
        ];
        let httpHeaderAcceptSelected: string = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected != undefined) {
            headers = headers.set("Accept", httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        let consumes: string[] = [
        ];

        return this.httpClient.get<any>(`${this.basePath}/tags`,
            {
                params: queryParameters,
                headers: headers,
                withCredentials: this.configuration.withCredentials,
            }
        );
    }

    /**
     * Delete Tag
     * Deletes a specific Tag.
     * @param id ID of the Tag to delete.
     * @param tenantId Id of the tenant being accessed
     */
    public tagsIdDelete(id: string, tenantId: string): Observable<{}> {
        if (id === null || id === undefined) {
            throw new Error('Required parameter id was null or undefined when calling tagsIdDelete.');
        }
        if (tenantId === null || tenantId === undefined) {
            throw new Error('Required parameter tenantId was null or undefined when calling tagsIdDelete.');
        }

        let headers = this.defaultHeaders;
        if (tenantId !== undefined && tenantId !== null) {
            headers = headers.set('TenantId', String(tenantId));
        }

        // to determine the Accept header
        let httpHeaderAccepts: string[] = [
            'application/json'
        ];
        let httpHeaderAcceptSelected: string = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected != undefined) {
            headers = headers.set("Accept", httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        let consumes: string[] = [
        ];

        return this.httpClient.delete<any>(`${this.basePath}/tags/${encodeURIComponent(String(id))}`,
            {
                headers: headers,
                withCredentials: this.configuration.withCredentials,
            }
        );
    }

    /**
     * Retrieve Tag
     * Retrieves a Tag with the specified ID.
     * @param id ID of the Tag to retrieve.
     * @param tenantId Id of the tenant being accessed
     */
    public tagsIdGet(id: string, tenantId: string): Observable<Tag> {
        if (id === null || id === undefined) {
            throw new Error('Required parameter id was null or undefined when calling tagsIdGet.');
        }
        if (tenantId === null || tenantId === undefined) {
            throw new Error('Required parameter tenantId was null or undefined when calling tagsIdGet.');
        }

        let headers = this.defaultHeaders;
        if (tenantId !== undefined && tenantId !== null) {
            headers = headers.set('TenantId', String(tenantId));
        }

        // to determine the Accept header
        let httpHeaderAccepts: string[] = [
            'application/json'
        ];
        let httpHeaderAcceptSelected: string = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected != undefined) {
            headers = headers.set("Accept", httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        let consumes: string[] = [
        ];

        return this.httpClient.get<any>(`${this.basePath}/tags/${encodeURIComponent(String(id))}`,
            {
                headers: headers,
                withCredentials: this.configuration.withCredentials,
            }
        );
    }

    /**
     * Update Tag
     * Updates the specified Tag.
     * @param id ID of the Tag to update.
     * @param tenantId Id of the tenant being accessed
     * @param tag New details of the Tag.
     */
    public tagsIdPut(id: string, tenantId: string, tag?: TagBase): Observable<{}> {
        if (id === null || id === undefined) {
            throw new Error('Required parameter id was null or undefined when calling tagsIdPut.');
        }
        if (tenantId === null || tenantId === undefined) {
            throw new Error('Required parameter tenantId was null or undefined when calling tagsIdPut.');
        }

        let headers = this.defaultHeaders;
        if (tenantId !== undefined && tenantId !== null) {
            headers = headers.set('TenantId', String(tenantId));
        }

        // to determine the Accept header
        let httpHeaderAccepts: string[] = [
            'application/json'
        ];
        let httpHeaderAcceptSelected: string = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected != undefined) {
            headers = headers.set("Accept", httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        let consumes: string[] = [
        ];
        let httpContentTypeSelected:string = this.configuration.selectHeaderContentType(consumes);
        if (httpContentTypeSelected != undefined) {
            headers = headers.set("Content-Type", httpContentTypeSelected);
        }

        return this.httpClient.put<any>(`${this.basePath}/tags/${encodeURIComponent(String(id))}`,
            tag,
            {
                headers: headers,
                withCredentials: this.configuration.withCredentials,
            }
        );
    }

    /**
     * Create Tag
     * Creates a new Tag.
     * @param tenantId Id of the tenant being accessed
     * @param tag Tag entity to add.
     */
    public tagsPost(tenantId: string, tag?: TagBase): Observable<NewEntityResponse> {
        if (tenantId === null || tenantId === undefined) {
            throw new Error('Required parameter tenantId was null or undefined when calling tagsPost.');
        }

        let headers = this.defaultHeaders;
        if (tenantId !== undefined && tenantId !== null) {
            headers = headers.set('TenantId', String(tenantId));
        }

        // to determine the Accept header
        let httpHeaderAccepts: string[] = [
            'application/json'
        ];
        let httpHeaderAcceptSelected: string = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected != undefined) {
            headers = headers.set("Accept", httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        let consumes: string[] = [
        ];
        let httpContentTypeSelected:string = this.configuration.selectHeaderContentType(consumes);
        if (httpContentTypeSelected != undefined) {
            headers = headers.set("Content-Type", httpContentTypeSelected);
        }

        return this.httpClient.post<any>(`${this.basePath}/tags`,
            tag,
            {
                headers: headers,
                withCredentials: this.configuration.withCredentials,
            }
        );
    }

}
