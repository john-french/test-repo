import { NgModule, ModuleWithProviders, SkipSelf, Optional } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HttpClientModule } from '@angular/common/http';
import { Configuration } from './configuration';

import { ActionInstancesService } from './api/actionInstances.service';
import { ActionMetadataService } from './api/actionMetadata.service';
import { ChannelsService } from './api/channels.service';
import { ConfigurationService } from './api/configuration.service';
import { DomainsService } from './api/domains.service';
import { MetadataImportService } from './api/metadataImport.service';
import { RecommendationsService } from './api/recommendations.service';
import { SelectionsService } from './api/selections.service';
import { StrategiesService } from './api/strategies.service';
import { SubjectMetadataService } from './api/subjectMetadata.service';
import { TagsService } from './api/tags.service';
import { TenantsService } from './api/tenants.service';

@NgModule({
  imports:      [ CommonModule, HttpClientModule ],
  declarations: [],
  exports:      [],
  providers: [
    ActionInstancesService,
    ActionMetadataService,
    ChannelsService,
    ConfigurationService,
    DomainsService,
    MetadataImportService,
    RecommendationsService,
    SelectionsService,
    StrategiesService,
    SubjectMetadataService,
    TagsService,
    TenantsService ]
})
export class ApiModule {
    public static forRoot(configurationFactory: () => Configuration): ModuleWithProviders {
        return {
            ngModule: ApiModule,
            providers: [ { provide: Configuration, useFactory: configurationFactory } ]
        }
    }

    constructor( @Optional() @SkipSelf() parentModule: ApiModule) {
        if (parentModule) {
            throw new Error('ApiModule is already loaded. Import your base AppModule only.');
        }
    }
}
