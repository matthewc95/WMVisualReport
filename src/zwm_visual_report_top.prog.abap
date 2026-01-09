*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_TOP
*&---------------------------------------------------------------------*
*& Global Data Definitions - Using ONLY primitive ABAP types
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Type Pools
*----------------------------------------------------------------------*
TYPE-POOLS: icon, col, slis.

*----------------------------------------------------------------------*
* Tables (for selection screen only)
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
* Types - Storage Bin Monitoring (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_bin,
    lgnum       TYPE char3,          " Warehouse Number
    lgtyp       TYPE char3,          " Storage Type
    lgpla       TYPE char10,         " Storage Bin
    lgber       TYPE char3,          " Storage Section
    lptyp       TYPE char4,          " Bin Type
    maxgew      TYPE p LENGTH 10 DECIMALS 3, " Maximum Weight
    maxle       TYPE i,              " Maximum Capacity
    anzle       TYPE i,              " Current LE count
    verme       TYPE p LENGTH 13 DECIMALS 3, " Available quantity
    gesme       TYPE p LENGTH 13 DECIMALS 3, " Total quantity
    einme       TYPE char3,          " Unit of entry
    occupancy   TYPE p LENGTH 5 DECIMALS 2, " Occupancy %
    status      TYPE char1,          " Status indicator
    status_icon TYPE char4,          " Icon for display
    matnr       TYPE char18,         " Material (if single)
    mat_count   TYPE i,              " Count of materials
    quant_count TYPE i,              " Count of quants
    blocked     TYPE char1,          " Blocked indicator
    color       TYPE char4,          " Row color
  END OF gty_storage_bin,

  gty_storage_bins TYPE STANDARD TABLE OF gty_storage_bin WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Transfer Orders (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_transfer_order,
    lgnum       TYPE char3,          " Warehouse Number
    tanum       TYPE char10,         " Transfer Order Number
    tapos       TYPE numc4,          " Item Number
    bwlvs       TYPE char3,          " Movement Type
    refnr       TYPE char10,         " Reference Number
    nltyp       TYPE char3,          " Destination Storage Type
    nlpla       TYPE char10,         " Destination Storage Bin
    vltyp       TYPE char3,          " Source Storage Type
    vlpla       TYPE char10,         " Source Storage Bin
    matnr       TYPE char18,         " Material
    werks       TYPE char4,          " Plant
    maktx       TYPE char40,         " Material Description
    vsolm       TYPE p LENGTH 13 DECIMALS 3, " Source Quantity
    meins       TYPE char3,          " Unit
    bdatu       TYPE sydatum,        " Creation Date
    bupts       TYPE syuzeit,        " Creation Time
    kdatu       TYPE sydatum,        " Confirmation Date
    kupts       TYPE syuzeit,        " Confirmation Time
    pession     TYPE char12,         " Confirmed by
    status      TYPE char10,         " Status text
    status_icon TYPE char4,          " Status icon
    wait_hours  TYPE p LENGTH 7 DECIMALS 2, " Hours waiting
    color       TYPE char4,          " Row color
    confirmed   TYPE char1,          " X = confirmed
  END OF gty_transfer_order,

  gty_transfer_orders TYPE STANDARD TABLE OF gty_transfer_order WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - KPIs (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_kpi_summary,
    description TYPE char60,
    value       TYPE char30,
    unit        TYPE char10,
    trend       TYPE char1,          " U=Up, D=Down, S=Stable
    trend_icon  TYPE char4,
    status      TYPE char1,
    status_icon TYPE char4,
  END OF gty_kpi_summary,

  gty_kpi_summaries TYPE STANDARD TABLE OF gty_kpi_summary WITH DEFAULT KEY.

TYPES:
  BEGIN OF gty_movement_kpi,
    lgnum           TYPE char3,
    bwlvs           TYPE char3,
    bwlvs_txt       TYPE char40,
    to_count        TYPE i,
    to_confirmed    TYPE i,
    to_open         TYPE i,
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    min_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    max_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    total_qty       TYPE p LENGTH 15 DECIMALS 3,
    status          TYPE char1,
    status_icon     TYPE char4,
  END OF gty_movement_kpi,

  gty_movement_kpis TYPE STANDARD TABLE OF gty_movement_kpi WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Workload Analysis (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_workload,
    lgnum        TYPE char3,
    date         TYPE sydatum,
    hour         TYPE i,
    to_created   TYPE i,
    to_confirmed TYPE i,
    to_open      TYPE i,
    items_moved  TYPE p LENGTH 15 DECIMALS 3,
    peak_flag    TYPE char1,
  END OF gty_workload,

  gty_workloads TYPE STANDARD TABLE OF gty_workload WITH DEFAULT KEY.

TYPES:
  BEGIN OF gty_user_workload,
    lgnum           TYPE char3,
    pession         TYPE char12,
    user_name       TYPE char80,
    to_confirmed    TYPE i,
    items_processed TYPE p LENGTH 15 DECIMALS 3,
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    efficiency      TYPE p LENGTH 5 DECIMALS 2,
  END OF gty_user_workload,

  gty_user_workloads TYPE STANDARD TABLE OF gty_user_workload WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Storage Type Summary (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_type_sum,
    lgnum         TYPE char3,
    lgtyp         TYPE char3,
    lgtyp_txt     TYPE char25,
    total_bins    TYPE i,
    occupied_bins TYPE i,
    empty_bins    TYPE i,
    blocked_bins  TYPE i,
    total_quants  TYPE i,
    occupancy_pct TYPE p LENGTH 5 DECIMALS 2,
    status        TYPE char1,
    status_icon   TYPE char4,
    bar_graph     TYPE char50,
  END OF gty_storage_type_sum,

  gty_storage_type_sums TYPE STANDARD TABLE OF gty_storage_type_sum WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Movement Simulation (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_movement_sim,
    lgnum         TYPE char3,
    tanum         TYPE char10,
    tapos         TYPE numc4,
    matnr         TYPE char18,
    maktx         TYPE char40,
    vsolm         TYPE p LENGTH 13 DECIMALS 3,
    meins         TYPE char3,
    vltyp         TYPE char3,
    vlpla         TYPE char10,
    nltyp         TYPE char3,
    nlpla         TYPE char10,
    bdatu         TYPE sydatum,
    bupts         TYPE syuzeit,
    timestamp     TYPE timestamp,
    direction     TYPE char30,
  END OF gty_movement_sim,

  gty_movement_sims TYPE STANDARD TABLE OF gty_movement_sim WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Daily Statistics (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_daily_stat,
    lgnum           TYPE char3,
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

  gty_daily_stats TYPE STANDARD TABLE OF gty_daily_stat WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Aging Analysis (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_aging,
    lgnum       TYPE char3,
    age_bucket  TYPE char20,
    to_count    TYPE i,
    percentage  TYPE p LENGTH 5 DECIMALS 2,
    bar_graph   TYPE char40,
  END OF gty_aging,

  gty_agings TYPE STANDARD TABLE OF gty_aging WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Material Flow (primitive types only)
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_material_flow,
    lgnum       TYPE char3,
    matnr       TYPE char18,
    maktx       TYPE char40,
    movements   TYPE i,
    total_qty   TYPE p LENGTH 15 DECIMALS 3,
    meins       TYPE char3,
    inbound     TYPE i,
    outbound    TYPE i,
    internal    TYPE i,
  END OF gty_material_flow,

  gty_material_flows TYPE STANDARD TABLE OF gty_material_flow WITH DEFAULT KEY.

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
  gv_total_bins        TYPE i,
  gv_occupied_bins     TYPE i,
  gv_empty_bins        TYPE i,
  gv_blocked_bins      TYPE i,
  gv_total_to          TYPE i,
  gv_open_to           TYPE i,
  gv_confirmed_to      TYPE i,
  gv_avg_confirm_time  TYPE p LENGTH 7 DECIMALS 2,
  gv_total_quants      TYPE i,
  gv_overall_occupancy TYPE p LENGTH 5 DECIMALS 2.

*----------------------------------------------------------------------*
* Field Symbols
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gs_storage_bin>    TYPE gty_storage_bin,
  <gs_transfer_order> TYPE gty_transfer_order.
