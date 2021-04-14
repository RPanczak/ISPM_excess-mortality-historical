# COVID-19 Dashboard Source Data

This documentation is also available at `https://www.covid19.admin.ch/api/data/documentation` including detailed html versions of the model documentation.

## Data
Check the `data` folder for the data source files.

### Files
| File  | Model  |  Description |
|---|---|---|
| COVID19Cases_geoRegion.(json/csv) | DailyIncomingData | Daily record timelines by geoRegion for cases. |
| COVID19Hosp_geoRegion.(json/csv) | DailyIncomingData | Daily record timelines by geoRegion for hospitalisations. |
| COVID19Death_geoRegion.(json/csv) | DailyIncomingData | Daily record timelines by geoRegion for deaths. |
| COVID19Test_geoRegion_all.(json/csv) | DailyIncomingData | Daily record timelines by geoRegion for tests (all test types). |
| COVID19Test_geoRegion_PCR_Antigen.(json/csv) | DailyIncomingData | Daily record timelines by geoRegion and test type (pcr/antigen) for tests. |
| COVID19Cases_geoRegion_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion for cases. |
| COVID19Hosp_geoRegion_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion for hospitalisations. |
| COVID19Death_geoRegion_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion for deaths. |
| COVID19Test_geoRegion_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion for tests. |
| COVID19Cases_geoRegion_AKL10_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and age brackets for cases. |
| COVID19Hosp_geoRegion_AKL10_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and age brackets for hospitalisations. |
| COVID19Death_geoRegion_AKL10_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and age brackets for deaths. |
| COVID19Test_geoRegion_AKL10_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and age brackets for tests (all test types). |
| COVID19Cases_geoRegion_sex_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and sex for cases. |
| COVID19Hosp_geoRegion_sex_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and sex for hospitalisations. |
| COVID19Death_geoRegion_sex_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and sex for deaths. |
| COVID19Test_geoRegion_sex_w.(json/csv) | WeeklyIncomingData | Iso-Week record timelines by geoRegion and sex for tests (all test types). |
| COVID19EvalTextDaily.(json/csv) | DailyReportIncomingData | Optional extra texts for daily report (PDF). |
| COVID19QuarantineIsolation_geoRegion_d.(json/csv) | ContactTracingIncomingData | Contact tracing data (current record by geoRegion where available). |
| COVID19HospCapacity_geoRegion.(json/csv) | HospCapacityDailyIncomingData | Daily hospital capacity data timelines by geoRegion. |
| COVID19IntQua.(json/csv) | InternationalQuarantineIncomingData | International quarantine data (mandatory quarantine requirement when entering Switzerland). |
| COVID19IntCases.(json/csv) | InternationalDailyIncomingData | International daily data (cases). |
| COVID19Re_geoRegion.(json/csv) | ReDailyIncomingData | Daily R<sub>e</sub> value data timelines by geoRegion. |
| COVID19VaccDosesDelivered.(json/csv) | VaccinationIncomingData | Vaccine doses delivered data by geoRegion. |
| COVID19VaccDosesAdministered.(json/csv) | VaccinationIncomingData | Vaccine doses administered data by geoRegion. |
| COVID19FullyVaccPersons.(json/csv) | VaccinationIncomingData | Fully vaccinated persons data by geoRegion. |
| COVID19VaccDosesAdministered_AKL10_w.(json/csv) | VaccinationWeeklyIncomingData | Iso-Week record timelines by geoRegion and age brackets for vaccine doses administered. |
| COVID19FullyVaccPersons_AKL10_w.(json/csv) | VaccinationWeeklyIncomingData | Iso-Week record timelines by geoRegion and age brackets for fully vaccinated persons. |
| COVID19VaccDosesAdministered_sex_w.(json/csv) VaccinationWeeklyIncomingData | Iso-Week record timelines by geoRegion and sex for vaccine doses administered. |
| COVID19FullyVaccPersons_sex_w.(json/csv) | VaccinationWeeklyIncomingData | Iso-Week record timelines by geoRegion and sex for fully vaccinated persons. |
| COVID19Variants.(json/csv) | VirusVariantsDailyIncomingData | Virus variant data by geoRegion. |

## Schema
Check the `sources.schema.json` file for schema information (only json-schema format for now).

Please note that the data schema can change in the future and be released in a new version. Changes will be tracked here and the current schema version can be read from the data context (see section Download Automation below).

### Releases

### v.0.4.3
**Released**: ` 19.03.2021`
**Description**:
- added data context history API, see documentation below for details
- added new properties `anteil_pos`, `lower_ci_day` and `upper_ci_day` to the `VirusVariantsDailyIncomingData` model

### v.0.4.2
**Released**: ` 26.02.2021`
**Description**:
- added new property `mean7d` to the `VaccinationIncomingData` model

### v.0.4.1
**Released**: ` 23.02.2021`
**Description**:
- added new weekly source files for fully vaccinated persons: `COVID19FullyVaccPerson_AKL10_ws.(json/csv)`, `COVID19FullyVaccPerson_sex_ws.(json/csv)`
- added new weekly source files for vaccination doses administered: `COVID19VaccDosesAdministered_AKL10_w.(json/csv)`, `COVID19VaccDosesAdministered_sex_w.(json/csv)`
- added model documentation `VaccinationWeeklyIncomingData`
- added new property `median_R_mean_mean7d` to R<sub>e</sub> data file `COVID19Re_geoRegion.(json/csv)`
- updated model documentation `ReDailyIncomingData`

### v.0.4.0
**Released**: `18.02.2021`
**Description**:
- added new source file for virus variant data: `COVID19Variants.(json/csv)`
- added model documentation `VirusVariantsDailyIncomingData`

### v.0.3.3
**Released**: `16.02.2021`
**Description**:
- added new source file for fully vaccinated persons: `COVID19FullyVaccPersons.(json/csv)`
- updated model documentation `VaccinationIncomingData`

#### v.0.3.2
**Released**: `05.02.2021`
**Description**:
- added type `COVID19VaccDosesReceived` data for CHFL to `COVID19VaccDosesDelivered.(json/csv)` (doses received by manufacturers)
- updated model documentation `VaccinationIncomingData`

#### v.0.3.1
**Released**: `28.01.2021`
**Description**:
- added new source files for vaccination data: `COVID19VaccDosesDelivered.(json/csv)`, `COVID19VaccDosesAdministered.(json/csv)`
- added new model documentation `VaccinationIncomingData`

#### v.0.3.0
**Released**: `13.01.2021`
**Description**:
- added new daily source file for international cases data `COVID19IntCases.(json/csv)`
- added new model documentation `InternationalDailyIncomingData`

#### v.0.2.0
**Released**: `17.12.2020`
**Description**:
- added new daily source file for R<sub>e</sub> Value by Cantons, CH and FL `COVID19Re_geoRegion.(json/csv)`
- added new source file for mandatory quarantine requirement when entering Switzerland `COVID19IntQua.(json/csv)`

#### v0.1.2

**Released**: `15.12.2020`

**Description**:
 - added new weekly source files for cases, hospitalisations, deaths and tests by geoRegion only
   - `COVID19Cases_geoRegion_w.(json/csv)`
   - `COVID19Hosp_geoRegion_w.(json/csv)`
   - `COVID19Death_geoRegion_w.(json/csv)`
   - `COVID19Test_geoRegion_w.(json/csv)`
 - added `default` weekly source file location group to `sources` of the data context for weekly data by geoRegion only
 - added new source file for daily hospital capacity data timelines by geoRegion `COVID19HospCapacity_geoRegion.(json/csv)`
 - added new model documentation for `HospCapacityDailyIncomingData`, check `https://www.covid19.admin.ch/api/data/documentation` for html version of model documentations
 - added `hospCapacity` file source location to `sources` of the data context
 - added fields `offset_Phase2b`, `sumTotal_Phase2b`, `inzsumTotal_Phase2b` and `anteil_pos_phase2b`to `DailyIncomingData`

#### v0.1.1
**Released**: `20.11.2020`

**Description**:
 - added new source file for test data by test type (pcr/antigen) `COVID19Test_geoRegion_PCR_Antigen.(json/csv)`
 - added `testPcrAntigen` file source location to `sources` of the data context
 - added fields `entries_pos` and `entries_neg` to DailyIncomingData

#### v0.1.0

**Released**: `05.11.2020`

**Description**: Initial version

## Data Context API

### Current Data Context
The current data context can be queried at a static location and provides information about the source date of the current data and source file locations.

```
GET https://www.covid19.admin.ch/api/data/context
```

### Data Model
`sourceDate` contains the overall source date of the data. Multiple publications per day are possible with the same `sourceDate`. Check the `dataVersion` to decide if you need to update your data.

`dataVersion` contains the current data version. Download links may be generated directly using the `dataVersion` but using the pre-generated urls in the `sources` field (see documentation below) is recommended.

`sources` contains information about the source location of all currently available raw source data (zip and individual files) to download as well as the schema version/content. OpenData DCAT-AP-CH metadata information will be published in the future in addition to this API to further facilitate automation of data downloads.

```
{
  "sources": {
    "schema": {
      "version": "{current-schema-version}",
      "jsonSchema": "{current-schema-location-url}"
    },
    "readme": "{current-readme-location-url}",
    "zip": {
      "json": "{current-source-location-url}",
      "csv": "{current-source-location-url}"
    },
    "individual": {
      "json": {
        "daily": {
          "cases": "{current-source-location-url}",
          "hosp": "{current-source-location-url}",
          "death": "{current-source-location-url}",
          "test": "{current-source-location-url}",
          "testPcrAntigen": "{current-source-location-url}",
          "hospCapacity": "{current-source-location-url}",
          "re": "{current-source-location-url}",
          "intCases": "{current-source-location-url}"
        },
        "weekly": {
          "byAge": {
            "cases": "{current-source-location-url}",
            "hosp": "{current-source-location-url}",
            "death": "{current-source-location-url}",
            "test": "{current-source-location-url}"
          },
          "bySex": {
            "cases": "{current-source-location-url}",
            "hosp": "{current-source-location-url}",
            "death": "{current-source-location-url}",
            "test": "{current-source-location-url}"
          },
          "default": {
            "cases": "{current-source-location-url}",
            "hosp": "{current-source-location-url}",
            "death": "{current-source-location-url}",
            "test": "{current-source-location-url}"
          },
        },
        "dailyReport": "{current-source-location-url}",
        "contactTracing": "{current-source-location-url}",
        "intQua": "{current-source-location-url}"
      },
      "csv": {
        "daily": {
          "cases": "{current-source-location-url}",
          "hosp": "{current-source-location-url}",
          "death": "{current-source-location-url}",
          "test": "{current-source-location-url}",
          "testPcrAntigen": "{current-source-location-url}",
          "hospCapacity": "{current-source-location-url}",
          "re": "{current-source-location-url}",
          "intCases": "{current-source-location-url}"
        },
        "weekly": {
          "byAge": {
            "cases": "{current-source-location-url}",
            "hosp": "{current-source-location-url}",
            "death": "{current-source-location-url}",
            "test": "{current-source-location-url}"
          },
          "bySex": {
            "cases": "{current-source-location-url}",
            "hosp": "{current-source-location-url}",
            "death": "{current-source-location-url}",
            "test": "{current-source-location-url}"
          },
          "default": {
            "cases": "{current-source-location-url}",
            "hosp": "{current-source-location-url}",
            "death": "{current-source-location-url}",
            "test": "{current-source-location-url}"
          },
        },
        "dailyReport": "{current-source-location-url}",
        "contactTracing": "{current-source-location-url}",
        "intQua": "{current-source-location-urxl}"
      }
    }
  }
}
```

### Data Context History

The data context history can be queried at a static location and provides a list of previously published data contexts.

```
GET https://www.covid19.admin.ch/api/data/context/history
```
Multiple publications per day are possible due to delays or data corrections etc. By default only the latest data context per day is returned from the API. Query `https://www.covid19.admin.ch/api/data/context/history/full` for all previously published data contexts (multiple per day returned).

### Data Model
`current` Url pointing to the current data context.

`documentation` Url of this documentation

`dataContexts` List of the individual data context history items

### Data Model (individual data context history item)
`date` Day of the publication, formatted as YYYY-MM-DD (e.g. 2021-03-18)

`latest` Multiple publications per day are possible due to delays or data corrections etc. This property indicates if the current data context is the latest one published for this day.

`published` Date and time of publication formatted as ISO 8601 string (e.g. 2021-03-18T13:30:35+01:00)

`dataVersion` Data version of thsi dataContext (see documentation above for details)

`dataContextUrl` Url pointing to the full data context object.

```
{
    "current": "https://www.covid19.admin.ch/api/data/context",
    "documentation": "https://www.covid19.admin.ch/api/data/documentation#data-context-history",
    "dataContexts: [
        {
            "date": "2021-03-18",
            "published": "2021-03-18T13:30:35+01:00",
            "latest": true,
            "dataVersion": "20210318-dec0fnrh",
            "dataContextUrl": "https://www.covid19.admin.ch/api/data/20210318-dec0fnrh/context"
        }
    ]
}
```
