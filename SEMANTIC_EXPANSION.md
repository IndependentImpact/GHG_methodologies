# Semantic Expansion — Feature Design

**Branch:** `semanticexpansion`
**Date:** 2026-06-25
**Status:** Draft

---

## 1. Purpose

The existing R packages evaluate applicability conditions quantitatively — capacity
thresholds, fractions, numeric bounds. Many applicability conditions in CDM methodology
documents are semantic: *"The project activity must implement a renewable energy technology
replacing a fossil fuel."* Those conditions cannot be evaluated without a shared vocabulary
where terms are formally defined and resolvable.

This branch adds the semantic layer: a CDM vocabulary, SHACL shapes per methodology, and
redesigned `check_applicability_*()` functions that validate a project's Fluree description
against those shapes — alongside the existing quantitative checks.

---

## 2. Ontology Cascade

Before defining any class or property, check whether it already exists in the following
stack. Only define what is genuinely absent at every level above.

| Level | Namespace | What it provides |
|---|---|---|
| AIAO | `http://w3id.org/aiao#` | `Activity`, `Agent`, `Instrument`, `Project`, `Objective`, `Role`, `Control` |
| impactont | `http://w3id.org/impactont#` | `State`, `Event`, `Impact`, `Indicator`, `SpatialLocation`, `hasModality` |
| claimont | `http://w3id.org/claimont#` | `Claim`, `Report`, `Predicate`, `Object` |
| infocomm | `http://w3id.org/infocomm#` | `Information` |
| indicatorOntology | `http://independentimpact.org/indicator-owl/` | `IndicatorDefinition`, `IndicatorFormula`, `Symbol` |
| methodologyOntology | `http://independentimpact.org/methodology/` | `Methodology`, `ApplicabilityCondition`, `EquationStep`, `Variable`, `VariableRole`, `MonitoringRequirement`, `ConstraintProfile` |
| **CDM vocabulary** | `http://independentimpact.org/cdm/` | CDM-specific classes and concept scheme — see §3 |
| Methodology-specific | `inst/` in each package | Only what is unique to that methodology |

Key mappings that eliminate otherwise tempting duplicates:
- `aiao:Instrument` covers `MethodologicalTool` (AIAO examples: boiler, turbine) — use as superclass
- `impactont:hasModality` (`"counterfactual"` / `"real"`) models the baseline/project state
  distinction directly — no new classes needed
- `aiao:Project` with `aiao:comprisesActivity` covers CDM project structure

---

## 3. CDM Vocabulary (new repo: `~/cdmVocabulary/`)

Namespace: `http://independentimpact.org/cdm/`

A standalone repo (not inside `GHG_methodologies`), consistent with the pattern of
`~/indicatorOntology/` and `~/methodologyOntology/`. Two files only.

### 3.1 `cdm.ttl` — OWL ontology (thin)

Defines only CDM-specific structural concepts not present in the cascade:

```turtle
@prefix cdm:  <http://independentimpact.org/cdm/> .
@prefix aiao: <http://w3id.org/aiao#> .
@prefix meth: <http://independentimpact.org/methodology/> .
```

| Class | Superclass | Rationale |
|---|---|---|
| `cdm:CDMProjectActivity` | `aiao:Activity` | CDM-registered project activity |
| `cdm:ProgrammeOfActivities` | `aiao:Project` | CDM PoA comprising CPAs |
| `cdm:ComponentProjectActivity` | `aiao:Activity` | CPA within a PoA |
| `cdm:CDMMethodologicalTool` | `aiao:Instrument` | UNFCCC-approved calculation tool used within a methodology |
| `cdm:CDMMethodology` | `meth:Methodology` | CDM-approved methodology instance |

Properties:
- `cdm:hasSectoralScope` — domain `cdm:CDMProjectActivity`, range `cdm:SectoralScopeConcept`
- `cdm:usesTool` — domain `cdm:CDMMethodology`, range `cdm:CDMMethodologicalTool`
- `cdm:calculatedExAnte` — datatype property, `xsd:boolean`, on `meth:Variable`

### 3.2 `cdm-concepts.ttl` — SKOS concept scheme

The controlled vocabulary referenced by SHACL applicability shapes. Covers the categories
that appear in methodology applicability conditions.

**Concept schemes:**

- `cdm:TechnologyTypeScheme` — renewable vs non-renewable, by category
  - `cdm:RenewableEnergyTechnology` (top concept)
    - `cdm:SolarPV`, `cdm:SolarThermal`, `cdm:WindTurbine`, `cdm:SmallHydro`,
      `cdm:GeothermalSystem`, `cdm:RenewableBiomassSystem`
  - `cdm:NonRenewableEnergyTechnology` (top concept)

- `cdm:EnergySourceScheme` — fuel and energy source types
  - `cdm:FossilFuel` (top concept)
    - `cdm:NaturalGas`, `cdm:Coal`, `cdm:Diesel`, `cdm:HeavyFuelOil`, `cdm:LPG`
  - `cdm:BiomassSource`
    - `cdm:RenewableBiomass`, `cdm:NonRenewableBiomass`

- `cdm:GridConnectionScheme`
  - `cdm:GridConnectedSystem`, `cdm:OffGridSystem`, `cdm:MiniGridSystem`, `cdm:CaptiveUseSystem`

- `cdm:SectoralScopeScheme` — UNFCCC CDM sectoral scopes 1–15
  - `cdm:SectoralScope1` (Energy industries), `cdm:SectoralScope4` (Manufacturing),
    `cdm:SectoralScope13` (Waste handling), etc.

- `cdm:ScaleScheme`
  - `cdm:LargeScale`, `cdm:SmallScale`

---

## 4. Per-Package Semantic Layer

Each methodology package gains two artefacts under `inst/`:

```
inst/
  shacl/
    <prefix>-applicability-shapes.ttl   # SHACL shapes for semantic applicability
  concepts/
    <prefix>-concepts.ttl               # methodology-specific SKOS extension (if needed)
```

The SHACL shapes import the CDM vocabulary and express the methodology's semantic
applicability conditions as `sh:NodeShape` constraints on `aiao:Activity` or
`cdm:CDMProjectActivity` focus nodes.

Example shape for AMS-I.A (technology must be renewable, ≤ 15 MW):

```turtle
@prefix cdm:  <http://independentimpact.org/cdm/> .
@prefix aiao: <http://w3id.org/aiao#> .
@prefix sh:   <http://www.w3.org/ns/shacl#> .

cdm:AmsIaApplicabilityShape a sh:NodeShape ;
  sh:targetClass cdm:CDMProjectActivity ;
  sh:property [
    sh:path aiao:isPerformedWith ;
    sh:class cdm:RenewableEnergyTechnology ;
    sh:minCount 1 ;
    sh:message "AMS-I.A: project must use a renewable energy technology."@en
  ] .
```

SKOS hierarchy traversal: before calling `shapeR::validate_shacl()`, a helper
materialises `skos:broader*` chains as `rdf:type` triples in the in-memory graph.
No changes required to `shapeR`.

---

## 5. Redesigned `check_applicability_*()` Functions

### 5.1 Dual interface

```r
check_applicability_renewable_technology(
  data,            # project IRI (chr) → fetches from Fluree, OR triples data.frame
  fluree_conn = NULL,
  shapes = read_methodology_shapes("cdmAcm0002", "technology")
)
```

- When `data` is a character IRI: fetches project triples from Fluree via `novaRush`,
  also fetches CDM concept scheme, materialises SKOS hierarchy, then validates.
- When `data` is a data.frame of triples: validates locally, no Fluree connection needed.
  Enables unit testing without a live database.

### 5.2 Return value

```r
list(
  conforms    = TRUE/FALSE,
  violations  = <data.frame>,   # shapeR validation results
  attestation = list(
    project_id   = <IRI>,
    methodology  = "AMS-I.A",
    condition    = "RenewableEnergyTechnology",
    checked_at   = <timestamp>,
    prima_facie  = TRUE/FALSE
  )
)
```

The attestation record is what gets transacted back into Fluree as part of the
validation workflow.

### 5.3 Quantitative conditions unchanged

Functions like `check_applicability_installed_capacity()` keep their existing
signatures. The semantic and quantitative checks are separate functions composed
by the meta-function.

---

## 6. Validation vs Verification

These are distinct stages with different data sources and different purposes.

**Validation (PDD stage)**
- Checks the *design* of the project against the methodology's applicability conditions
- Data source: project entity in Fluree (AIAO + CDM + methodology vocabulary)
- Mechanism: SHACL shapes from `inst/shacl/` run against Fluree triples
- Outcome: prima facie compliance + PP attestation recorded in Fluree
- R entry point: `check_applicability_*()` functions

**Verification (post-implementation)**
- Checks *reported results* against the monitoring plan in the approved PDD
- Data source: monitoring observations in Fluree, extracted by the monitoring plan's queries
- Mechanism: monitoring plan queries produce tidy dataframes → `calculate_*()` functions
- Outcome: verified emission reductions

The R methodology packages are responsible for validation (SHACL shapes) and
the calculation logic (equations). They do not own the monitoring plan queries —
those are project-specific.

---

## 7. Monitoring Plan and Variable Registry

The monitoring plan (part of the approved PDD, project-specific) contains the
Fluree queries that extract monitoring data in the format expected by `calculate_*()`
functions. The methodology package defines the expected format via the variable
registry — this is the contract between the monitoring plan and the R functions.

Each package gains a `variable_registry_<prefix>()` function (in `*_ontology.R`)
mapping R column names to ontology IRIs, roles, and QUDT units:

```r
variable_registry_ams_ia <- function() {
  tibble::tribble(
    ~r_name,                   ~ontology_iri,                        ~role,                ~unit_qudt,
    "generation_kwh",          meth_var_iri("AmsIa", "EG"),          METH_MEASURED_INPUT,  QUDT_UNIT_KWH,
    "baseline_generation_kwh", meth_var_iri("AmsIa", "EG_baseline"), METH_DERIVED_OUTPUT,  QUDT_UNIT_KWH,
    "baseline_emissions",      meth_var_iri("AmsIa", "BE"),          METH_DERIVED_OUTPUT,  QUDT_UNIT_TCO2E,
    "emission_reductions",     meth_var_iri("AmsIa", "ER"),          METH_REPORTED_OUTPUT, QUDT_UNIT_TCO2E
  )
}
```

This is the machine-readable counterpart of `VARIABLE_REGISTRY.md`. The monitoring plan
author uses it to know which columns the Fluree query must produce.

---

## 8. Meta-Function

```r
estimate_emission_reductions_ams_ia(
  monitoring_data,               # tidy data.frame from monitoring plan query
  grid_emission_factor,
  project_emission_factor = 0,
  group_cols              = NULL,
  validate_applicability  = FALSE,  # set TRUE to run §5 checks first
  project_id              = NULL,   # required when validate_applicability = TRUE
  fluree_conn             = NULL    # required when project_id is an IRI
)
```

When `validate_applicability = FALSE` (default), the function assumes the methodology
is applicable and proceeds directly to calculations. When `TRUE`, it runs all
`check_applicability_*()` functions and stops with an informative error if any
semantic or quantitative condition fails.

---

## 9. Dependencies and Sequencing

| Step | Deliverable | Depends on |
|---|---|---|
| 1 | `~/cdmVocabulary/` repo — `cdm.ttl` + `cdm-concepts.ttl` | Nothing |
| 2 | Update `novaRush` for Fluree v4 API | Nothing |
| 3 | SKOS hierarchy materialisation helper | `shapeR` (already available) |
| 4 | `inst/shacl/` + `inst/concepts/` for pilot packages (`cdmAmsIa`, `cdmAcm0002`) | Step 1 |
| 5 | `variable_registry_*()` for pilot packages | — |
| 6 | Redesigned `check_applicability_*()` for pilots | Steps 2, 3, 4 |
| 7 | `validate_applicability` flag in meta-functions | Step 6 |
| 8 | Roll out to remaining packages | Steps 4–7 proven on pilots |

---

## 10. Open Questions

- **novaRush v4**: Fluree v4 changed the API significantly. The update scope needs to
  be assessed before Step 2 can be sized.

- **SHACL shapes per methodology**: The exact conditions to encode as shapes must be
  read from each methodology document. The pilot packages (`cdmAmsIa`, `cdmAcm0002`)
  will establish the pattern; the remainder follow.

- **Concept scheme completeness**: `cdm-concepts.ttl` starts with the concepts needed
  for the pilot packages' applicability conditions. It grows as further packages are
  added — this is expected and correct.

- **Attestation storage**: The attestation record (§5.2) must be transacted into Fluree.
  The transaction schema (what ledger, what predicates) is outside this document's scope
  and depends on the broader project platform design.
