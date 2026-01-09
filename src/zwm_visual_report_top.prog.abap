*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_TOP
*&---------------------------------------------------------------------*
*& Global Data Definitions, Types, Constants
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Type Pools
*----------------------------------------------------------------------*
TYPE-POOLS: icon, col, slis.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: t300,      " Warehouse Numbers
        t301,      " Storage Types
        lagp,      " Storage Bins
        lqua,      " Quants
        ltap,      " Transfer Order Items
        ltak.      " Transfer Order Headers

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS:
  " Screen numbers
  gc_dynnr_main     TYPE sy-dynnr VALUE '0100',
  gc_dynnr_popup    TYPE sy-dynnr VALUE '0200',

  " Status indicators
  gc_status_green   TYPE char1 VALUE '3',  " Good
  gc_status_yellow  TYPE char1 VALUE '2',  " Warning
  gc_status_red     TYPE char1 VALUE '1',  " Critical

  " Tab indices
  gc_tab_overview   TYPE i VALUE 1,
  gc_tab_to         TYPE i VALUE 2,
  gc_tab_bins       TYPE i VALUE 3,
  gc_tab_kpi        TYPE i VALUE 4,
  gc_tab_simulation TYPE i VALUE 5,
  gc_tab_workload   TYPE i VALUE 6,

  " Traffic light thresholds
  gc_occupancy_high TYPE p DECIMALS 2 VALUE '85.00',
  gc_occupancy_med  TYPE p DECIMALS 2 VALUE '60.00',

  " Time thresholds for TO confirmation (in hours)
  gc_to_time_good   TYPE i VALUE 4,
  gc_to_time_warn   TYPE i VALUE 8,

  " Maximum records for performance
  gc_max_records    TYPE i VALUE 100000.

*----------------------------------------------------------------------*
* Types - Storage Bin Monitoring
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_bin,
    lgnum       TYPE lgnum,         " Warehouse Number
    lgtyp       TYPE lgtyp,         " Storage Type
    lgpla       TYPE lgpla,         " Storage Bin
    lgber       TYPE lgber,         " Storage Section
    lptyp       TYPE char4,         " Bin Type
    maxgew      TYPE p LENGTH 10 DECIMALS 3, " Maximum Weight
    maxle       TYPE i,             " Maximum Capacity
    anzle       TYPE i,             " Current LE count
    verme       TYPE menge_d,       " Available quantity
    gesme       TYPE menge_d,       " Total quantity
    einme       TYPE meins,         " Unit of entry
    occupancy   TYPE p LENGTH 5 DECIMALS 2, " Occupancy %
    status      TYPE char1,         " Status indicator
    status_icon TYPE icon_d,        " Icon for display
    matnr       TYPE matnr,         " Material (if single)
    mat_count   TYPE i,             " Count of materials
    quant_count TYPE i,             " Count of quants
    blocked     TYPE char1,         " Blocked indicator
    color       TYPE char4,         " Row color
  END OF gty_storage_bin,

  gty_storage_bins TYPE STANDARD TABLE OF gty_storage_bin WITH KEY lgnum lgtyp lgpla.

*----------------------------------------------------------------------*
* Types - Transfer Orders
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_transfer_order,
    lgnum       TYPE lgnum,         " Warehouse Number
    tanum       TYPE tanum,         " Transfer Order Number
    tapos       TYPE tapos,         " Item Number
    bwlvs       TYPE bwlvs,         " Movement Type
    betyp       TYPE ltap-betyp,    " Source Document Type
    benum       TYPE ltap-benum,    " Source Document
    nltyp       TYPE nltyp,         " Destination Storage Type
    nlpla       TYPE nlpla,         " Destination Storage Bin
    vltyp       TYPE vltyp,         " Source Storage Type
    vlpla       TYPE vlpla,         " Source Storage Bin
    matnr       TYPE matnr,         " Material
    werks       TYPE werks_d,       " Plant
    maktx       TYPE maktx,         " Material Description
    vsolm       TYPE ltap-vsolm,    " Source Quantity
    vsola       TYPE ltap-vsola,    " Source Qty Alt. UOM
    meins       TYPE meins,         " Unit
    bdatu       TYPE ltap-bdatu,    " Creation Date
    bupts       TYPE ltap-bupts,    " Creation Time
    kdatu       TYPE ltap-kdatu,    " Confirmation Date
    kupts       TYPE ltap-kupts,    " Confirmation Time
    pession     TYPE ltap-pession,  " Confirmed by
    status      TYPE char10,        " Status text
    status_icon TYPE icon_d,        " Status icon
    wait_hours  TYPE p LENGTH 7 DECIMALS 2, " Hours waiting
    color       TYPE char4,         " Row color
    confirmed   TYPE char1,         " X = confirmed
  END OF gty_transfer_order,

  gty_transfer_orders TYPE STANDARD TABLE OF gty_transfer_order WITH KEY lgnum tanum tapos.

*----------------------------------------------------------------------*
* Types - KPIs
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_kpi_summary,
    description TYPE char60,
    value       TYPE char30,
    unit        TYPE char10,
    trend       TYPE char1,         " U=Up, D=Down, S=Stable
    trend_icon  TYPE icon_d,
    status      TYPE char1,
    status_icon TYPE icon_d,
  END OF gty_kpi_summary,

  gty_kpi_summaries TYPE STANDARD TABLE OF gty_kpi_summary WITH EMPTY KEY.

TYPES:
  BEGIN OF gty_movement_kpi,
    lgnum           TYPE lgnum,
    bwlvs           TYPE bwlvs,
    bwlvs_txt       TYPE t333t-btext,
    to_count        TYPE i,
    to_confirmed    TYPE i,
    to_open         TYPE i,
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    min_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    max_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    total_qty       TYPE p LENGTH 15 DECIMALS 3,
    status          TYPE char1,
    status_icon     TYPE icon_d,
  END OF gty_movement_kpi,

  gty_movement_kpis TYPE STANDARD TABLE OF gty_movement_kpi WITH KEY lgnum bwlvs.

*----------------------------------------------------------------------*
* Types - Workload Analysis
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_workload,
    lgnum       TYPE lgnum,
    date        TYPE sydatum,
    hour        TYPE i,
    to_created  TYPE i,
    to_confirmed TYPE i,
    to_open     TYPE i,
    items_moved TYPE p LENGTH 15 DECIMALS 3,
    peak_flag   TYPE char1,
  END OF gty_workload,

  gty_workloads TYPE STANDARD TABLE OF gty_workload WITH KEY lgnum date hour.

TYPES:
  BEGIN OF gty_user_workload,
    lgnum           TYPE lgnum,
    pession         TYPE ltap-pession,
    user_name       TYPE char80,
    to_confirmed    TYPE i,
    items_processed TYPE p LENGTH 15 DECIMALS 3,
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    efficiency      TYPE p LENGTH 5 DECIMALS 2,
  END OF gty_user_workload,

  gty_user_workloads TYPE STANDARD TABLE OF gty_user_workload WITH KEY lgnum pession.

*----------------------------------------------------------------------*
* Types - Storage Type Summary
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_type_sum,
    lgnum         TYPE lgnum,
    lgtyp         TYPE lgtyp,
    lgtyp_txt     TYPE t301t-ltypt,
    total_bins    TYPE i,
    occupied_bins TYPE i,
    empty_bins    TYPE i,
    blocked_bins  TYPE i,
    total_quants  TYPE i,
    occupancy_pct TYPE p LENGTH 5 DECIMALS 2,
    status        TYPE char1,
    status_icon   TYPE icon_d,
    bar_graph     TYPE char50,  " ASCII progress bar
  END OF gty_storage_type_sum,

  gty_storage_type_sums TYPE STANDARD TABLE OF gty_storage_type_sum WITH KEY lgnum lgtyp.

*----------------------------------------------------------------------*
* Types - Movement Simulation
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_movement_sim,
    lgnum         TYPE lgnum,
    tanum         TYPE tanum,
    tapos         TYPE tapos,
    matnr         TYPE matnr,
    maktx         TYPE maktx,
    vsolm         TYPE ltap-vsolm,
    meins         TYPE meins,
    vltyp         TYPE vltyp,
    vlpla         TYPE vlpla,
    nltyp         TYPE nltyp,
    nlpla         TYPE nlpla,
    bdatu         TYPE ltap-bdatu,
    bupts         TYPE ltap-bupts,
    timestamp     TYPE timestamp,
    direction     TYPE char30,    " Arrow representation
  END OF gty_movement_sim,

  gty_movement_sims TYPE STANDARD TABLE OF gty_movement_sim WITH KEY timestamp lgnum tanum tapos.

*----------------------------------------------------------------------*
* Types - Daily Statistics
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_daily_stat,
    lgnum           TYPE lgnum,
    date            TYPE sydatum,
    day_name        TYPE char10,
    to_created      TYPE i,
    to_confirmed    TYPE i,
    avg_confirm_hrs TYPE p LENGTH 7 DECIMALS 2,
    total_qty       TYPE p LENGTH 15 DECIMALS 3,
    peak_hour       TYPE i,
    bar_created     TYPE char30,
    bar_confirmed   TYPE char30,
  END OF gty_daily_stat,

  gty_daily_stats TYPE STANDARD TABLE OF gty_daily_stat WITH KEY lgnum date.

*----------------------------------------------------------------------*
* Types - Aging Analysis
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_aging,
    lgnum       TYPE lgnum,
    age_bucket  TYPE char20,
    to_count    TYPE i,
    percentage  TYPE p LENGTH 5 DECIMALS 2,
    bar_graph   TYPE char40,
  END OF gty_aging,

  gty_agings TYPE STANDARD TABLE OF gty_aging WITH KEY lgnum age_bucket.

*----------------------------------------------------------------------*
* Types - Material Flow
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_material_flow,
    lgnum       TYPE lgnum,
    matnr       TYPE matnr,
    maktx       TYPE maktx,
    movements   TYPE i,
    total_qty   TYPE p LENGTH 15 DECIMALS 3,
    meins       TYPE meins,
    inbound     TYPE i,
    outbound    TYPE i,
    internal    TYPE i,
  END OF gty_material_flow,

  gty_material_flows TYPE STANDARD TABLE OF gty_material_flow WITH KEY lgnum matnr.

*----------------------------------------------------------------------*
* Global Variables - Data
*----------------------------------------------------------------------*
DATA:
  gt_storage_bins     TYPE gty_storage_bins,
  gt_transfer_orders  TYPE gty_transfer_orders,
  gt_kpi_summary      TYPE gty_kpi_summaries,
  gt_movement_kpis    TYPE gty_movement_kpis,
  gt_workloads        TYPE gty_workloads,
  gt_user_workloads   TYPE gty_user_workloads,
  gt_storage_type_sum TYPE gty_storage_type_sums,
  gt_movement_sims    TYPE gty_movement_sims,
  gt_daily_stats      TYPE gty_daily_stats,
  gt_agings           TYPE gty_agings,
  gt_material_flows   TYPE gty_material_flows.

*----------------------------------------------------------------------*
* Global Variables - GUI Objects
*----------------------------------------------------------------------*
DATA:
  go_dock_container   TYPE REF TO cl_gui_docking_container,
  go_splitter         TYPE REF TO cl_gui_splitter_container,
  go_container_left   TYPE REF TO cl_gui_container,
  go_container_right  TYPE REF TO cl_gui_container,
  go_container_top    TYPE REF TO cl_gui_container,
  go_container_bottom TYPE REF TO cl_gui_container,
  go_html_viewer      TYPE REF TO cl_gui_html_viewer,
  go_tab_container    TYPE REF TO cl_gui_custom_container,
  go_alv_container    TYPE REF TO cl_gui_custom_container,
  go_alv_grid         TYPE REF TO cl_gui_alv_grid,
  go_salv_table       TYPE REF TO cl_salv_table.

*----------------------------------------------------------------------*
* Global Variables - Control
*----------------------------------------------------------------------*
DATA:
  gv_okcode           TYPE sy-ucomm,
  gv_active_tab       TYPE i VALUE 1,
  gv_initialized      TYPE abap_bool,
  gv_data_loaded      TYPE abap_bool.

*----------------------------------------------------------------------*
* Global Variables - Dashboard Metrics
*----------------------------------------------------------------------*
DATA:
  gv_total_bins       TYPE i,
  gv_occupied_bins    TYPE i,
  gv_empty_bins       TYPE i,
  gv_blocked_bins     TYPE i,
  gv_total_to         TYPE i,
  gv_open_to          TYPE i,
  gv_confirmed_to     TYPE i,
  gv_avg_confirm_time TYPE p LENGTH 7 DECIMALS 2,
  gv_total_quants     TYPE i,
  gv_overall_occupancy TYPE p LENGTH 5 DECIMALS 2.

*----------------------------------------------------------------------*
* Field Symbols
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gs_storage_bin>    TYPE gty_storage_bin,
  <gs_transfer_order> TYPE gty_transfer_order.
