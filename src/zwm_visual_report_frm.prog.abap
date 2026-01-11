*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_FRM
*&---------------------------------------------------------------------*
*& Forms, Subroutines, and Helper Methods
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  " Set default date range (last 30 days)
  p_datfr = sy-datum - 30.
  p_datto = sy-datum.

  " Set max records
  p_max = gc_max_records.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM at_selection_screen_output.
  " Modify selection screen dynamically if needed
  LOOP AT SCREEN.
    " Example: Hide certain fields based on mode
    IF rb_sim = abap_true AND screen-group1 = 'BIN'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_WAREHOUSE
*&---------------------------------------------------------------------*
FORM f4_warehouse.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname     = 'T300'
      fieldname   = 'LGNUM'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_LGNUM-LOW'
    TABLES
      return_tab  = lt_return
    EXCEPTIONS
      OTHERS      = 1.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_STORAGE_TYPE
*&---------------------------------------------------------------------*
FORM f4_storage_type.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        lt_t301   TYPE STANDARD TABLE OF t301.

  " Get storage types for selected warehouse
  IF s_lgnum[] IS NOT INITIAL.
    SELECT lgnum, lgtyp
      FROM t301
      INTO TABLE @lt_t301
      WHERE lgnum IN @s_lgnum.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'LGTYP'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'S_LGTYP-LOW'
        value_org   = 'S'
      TABLES
        value_tab   = lt_t301
        return_tab  = lt_return
      EXCEPTIONS
        OTHERS      = 1.
  ELSE.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname     = 'T301'
        fieldname   = 'LGTYP'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'S_LGTYP-LOW'
      TABLES
        return_tab  = lt_return
      EXCEPTIONS
        OTHERS      = 1.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_STORAGE_BIN
*&---------------------------------------------------------------------*
FORM f4_storage_bin.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        lt_lagp   TYPE STANDARD TABLE OF lagp.

  " Get storage bins for selected warehouse/type
  SELECT lgnum, lgtyp, lgpla
    FROM lagp
    INTO CORRESPONDING FIELDS OF TABLE @lt_lagp
    WHERE lgnum IN @s_lgnum
      AND lgtyp IN @s_lgtyp
    UP TO 1000 ROWS.

  IF lt_lagp IS NOT INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'LGPLA'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'S_LGPLA-LOW'
        value_org   = 'S'
      TABLES
        value_tab   = lt_lagp
        return_tab  = lt_return
      EXCEPTIONS
        OTHERS      = 1.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection.
  DATA: lo_controller TYPE REF TO lcl_controller.

  " Validate selections
  IF s_lgnum[] IS INITIAL.
    MESSAGE 'Please enter at least one warehouse number' TYPE 'E'.
    RETURN.
  ENDIF.

  IF p_datfr > p_datto.
    MESSAGE 'Date From cannot be greater than Date To' TYPE 'E'.
    RETURN.
  ENDIF.

  " Show progress
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Initializing...'.

  " Get controller instance
  lo_controller = lcl_controller=>get_instance( ).

  " Initialize with selection parameters
  lo_controller->initialize(
    it_lgnum     = s_lgnum[]
    it_lgtyp     = s_lgtyp[]
    it_lgpla     = s_lgpla[]
    it_matnr     = s_matnr[]
    it_bwlvs     = s_bwlvs[]
    iv_date_from = p_datfr
    iv_date_to   = p_datto ).

  " Show progress
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 30
      text       = 'Loading data...'.

  " Generate test data if requested
  IF p_test = abap_true.
    PERFORM generate_test_data.
  ENDIF.

  " Load all data
  lo_controller->load_all_data( ).

  " Show progress
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = 'Preparing display...'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM end_of_selection.
  DATA: lo_controller TYPE REF TO lcl_controller,
        lo_alv        TYPE REF TO lcl_alv_handler.

  lo_controller = lcl_controller=>get_instance( ).
  lo_alv = lcl_alv_handler=>get_instance( ).

  " Determine which view to display based on radio button selection
  IF rb_over = abap_true.
    " Display Overview Dashboard
    lo_controller->display_overview( ).

    " Also show storage type summary as ALV
    SKIP 2.
    WRITE: / 'Storage Type Summary:'.
    SKIP.
    lo_alv->display_storage_type_summary( gt_storage_type_sum ).

  ELSEIF rb_bins = abap_true.
    " Display Storage Bins
    lo_alv->display_storage_bins( gt_storage_bins ).

  ELSEIF rb_to = abap_true.
    " Display Transfer Orders
    lo_alv->display_transfer_orders( gt_transfer_orders ).

  ELSEIF rb_kpi = abap_true.
    " Display KPI Dashboard
    PERFORM display_kpi_dashboard.

  ELSEIF rb_sim = abap_true.
    " Display Movement Simulation
    PERFORM display_simulation.

  ELSEIF rb_work = abap_true.
    " Display Workload Analysis
    PERFORM display_workload_analysis.

  ENDIF.

  " Show completion
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Complete'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_KPI_DASHBOARD
*&---------------------------------------------------------------------*
FORM display_kpi_dashboard.
  DATA: lo_alv TYPE REF TO lcl_alv_handler.
  lo_alv = lcl_alv_handler=>get_instance( ).

  " Write KPI Header
  WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
  WRITE: / '                         KPI STATISTICS DASHBOARD'.
  WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
  SKIP.

  " Movement Type KPIs Section
  FORMAT COLOR COL_HEADING.
  WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
  WRITE: / '│                     MOVEMENT TYPE PERFORMANCE KPIs                          │'.
  WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
  FORMAT COLOR OFF.
  SKIP.

  lo_alv->display_movement_kpis( gt_movement_kpis ).

  SKIP 2.

  " Daily Statistics Section
  FORMAT COLOR COL_HEADING.
  WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
  WRITE: / '│                         DAILY STATISTICS                                    │'.
  WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
  FORMAT COLOR OFF.
  SKIP.

  lo_alv->display_daily_statistics( gt_daily_stats ).

  SKIP 2.

  " Aging Analysis Section
  IF gt_agings IS NOT INITIAL.
    FORMAT COLOR COL_HEADING.
    WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
    WRITE: / '│                    OPEN TRANSFER ORDER AGING ANALYSIS                       │'.
    WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
    FORMAT COLOR OFF.
    SKIP.

    lo_alv->display_aging_analysis( gt_agings ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_SIMULATION
*&---------------------------------------------------------------------*
FORM display_simulation.
  DATA: lo_alv TYPE REF TO lcl_alv_handler.
  lo_alv = lcl_alv_handler=>get_instance( ).

  " Write Simulation Header
  WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
  WRITE: / '                      MOVEMENT SIMULATION TIMELINE'.
  WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
  SKIP.

  " Show material flow summary first
  FORMAT COLOR COL_HEADING.
  WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
  WRITE: / '│                         MATERIAL FLOW SUMMARY                               │'.
  WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
  FORMAT COLOR OFF.
  SKIP.

  lo_alv->display_material_flow( gt_material_flows ).

  SKIP 2.

  " Movement Timeline
  FORMAT COLOR COL_HEADING.
  WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
  WRITE: / '│                         MOVEMENT TIMELINE                                   │'.
  WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
  FORMAT COLOR OFF.
  SKIP.

  " Show visual timeline representation
  PERFORM display_visual_timeline.

  SKIP.

  " Detailed movement list
  lo_alv->display_movement_simulation( gt_movement_sims ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_VISUAL_TIMELINE
*&---------------------------------------------------------------------*
FORM display_visual_timeline.
  DATA: lv_prev_date TYPE sydatum,
        lv_counter   TYPE i.

  " Show first 50 movements visually
  LOOP AT gt_movement_sims INTO DATA(ls_sim).
    lv_counter = lv_counter + 1.
    IF lv_counter > 50.
      WRITE: / '... (showing first 50 movements, see full list below)'.
      EXIT.
    ENDIF.

    " Date separator
    IF ls_sim-bdatu <> lv_prev_date.
      SKIP.
      FORMAT COLOR COL_GROUP.
      WRITE: / '──────────────', ls_sim-bdatu, '──────────────'.
      FORMAT COLOR OFF.
      lv_prev_date = ls_sim-bdatu.
    ENDIF.

    " Movement line with visual indicator
    WRITE: / ls_sim-bzeit+0(5),          " Time (HH:MM)
             icon_led_green AS ICON,      " Movement icon
             ls_sim-matnr,                " Material
             ls_sim-direction,            " From -> To
             ls_sim-vsolm DECIMALS 2,     " Quantity
             ls_sim-meins.                " Unit

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_WORKLOAD_ANALYSIS
*&---------------------------------------------------------------------*
FORM display_workload_analysis.
  DATA: lo_alv TYPE REF TO lcl_alv_handler.
  lo_alv = lcl_alv_handler=>get_instance( ).

  " Write Workload Header
  WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
  WRITE: / '                         WORKLOAD ANALYSIS'.
  WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
  SKIP.

  " User Performance Section
  FORMAT COLOR COL_HEADING.
  WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
  WRITE: / '│                      USER PERFORMANCE ANALYSIS                              │'.
  WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
  FORMAT COLOR OFF.
  SKIP.

  lo_alv->display_user_workload( gt_user_workloads ).

  SKIP 2.

  " Hourly Distribution
  FORMAT COLOR COL_HEADING.
  WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
  WRITE: / '│                     HOURLY ACTIVITY DISTRIBUTION                            │'.
  WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
  FORMAT COLOR OFF.
  SKIP.

  PERFORM display_hourly_heatmap.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_HOURLY_HEATMAP
*&---------------------------------------------------------------------*
FORM display_hourly_heatmap.
  DATA: lt_hourly TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        lv_max    TYPE i.

  " Initialize hours array (0-23)
  DO 24 TIMES.
    APPEND 0 TO lt_hourly.
  ENDDO.

  " Aggregate by hour
  LOOP AT gt_workloads INTO DATA(ls_wl).
    DATA(lv_idx) = ls_wl-hour + 1.
    IF lv_idx BETWEEN 1 AND 24.
      lt_hourly[ lv_idx ] = lt_hourly[ lv_idx ] + ls_wl-to_created.
    ENDIF.
  ENDLOOP.

  " Find max for scaling
  LOOP AT lt_hourly INTO DATA(lv_val).
    IF lv_val > lv_max.
      lv_max = lv_val.
    ENDIF.
  ENDLOOP.

  " Display heat map
  WRITE: / 'Hour:'.
  DO 24 TIMES.
    DATA(lv_hour) = sy-index - 1.
    WRITE: AT 8 lv_hour NO-GAP, '|' NO-GAP.
  ENDDO.
  NEW-LINE.

  WRITE: / 'TOs: '.
  LOOP AT lt_hourly INTO lv_val.
    DATA(lv_bar_len) = 0.
    IF lv_max > 0.
      lv_bar_len = ( lv_val / lv_max ) * 10.
    ENDIF.

    " Color based on intensity
    IF lv_bar_len >= 8.
      FORMAT COLOR COL_NEGATIVE.
    ELSEIF lv_bar_len >= 5.
      FORMAT COLOR COL_TOTAL.
    ELSEIF lv_bar_len >= 2.
      FORMAT COLOR COL_POSITIVE.
    ELSE.
      FORMAT COLOR OFF.
    ENDIF.

    WRITE: lv_val NO-GAP, '|' NO-GAP.
    FORMAT COLOR OFF.
  ENDLOOP.
  NEW-LINE.

  " Legend
  SKIP.
  WRITE: / 'Legend:'.
  FORMAT COLOR COL_POSITIVE.
  WRITE: 'Low'.
  FORMAT COLOR COL_TOTAL.
  WRITE: 'Medium'.
  FORMAT COLOR COL_NEGATIVE.
  WRITE: 'High (Peak)'.
  FORMAT COLOR OFF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GENERATE_TEST_DATA
*&---------------------------------------------------------------------*
FORM generate_test_data.
  " This form would generate test data for demonstration purposes
  " In a real scenario, this would be disabled or removed

  MESSAGE 'Test data generation is a placeholder - use real WM data' TYPE 'S'.

  " Note: Test data generation should be done at database level
  " or via transaction LT01/LT02 for transfer orders
  " This is just a placeholder for the framework

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLEANUP_GUI_OBJECTS
*&---------------------------------------------------------------------*
FORM cleanup_gui_objects.
  " Free GUI objects
  IF go_html_viewer IS NOT INITIAL.
    go_html_viewer->free( ).
    FREE go_html_viewer.
  ENDIF.

  IF go_dock_container IS NOT INITIAL.
    go_dock_container->free( ).
    FREE go_dock_container.
  ENDIF.

  IF go_alv_container IS NOT INITIAL.
    go_alv_container->free( ).
    FREE go_alv_container.
  ENDIF.

  IF go_alv_grid IS NOT INITIAL.
    go_alv_grid->free( ).
    FREE go_alv_grid.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXPORT_TO_EXCEL
*&---------------------------------------------------------------------*
FORM export_to_excel.
  DATA: lo_alv TYPE REF TO lcl_alv_handler.

  " Export current view to Excel
  " Using SAP standard functionality via ALV

  MESSAGE 'Use ALV toolbar Excel export function' TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PRINT_REPORT
*&---------------------------------------------------------------------*
FORM print_report.
  " Print current report
  " Using SAP standard print functionality

  NEW-PAGE PRINT ON NO DIALOG.

  " Re-display current view for printing
  DATA(lo_controller) = lcl_controller=>get_instance( ).
  lo_controller->display_overview( ).

  NEW-PAGE PRINT OFF.

  MESSAGE 'Report sent to print spool' TYPE 'S'.
ENDFORM.
