*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_CLS
*&---------------------------------------------------------------------*
*& Local Classes Definition and Implementation
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* CLASS lcl_utilities DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_utilities DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      " Conversion methods
      get_status_icon
        IMPORTING iv_status       TYPE char1
        RETURNING VALUE(rv_icon)  TYPE char4,

      get_trend_icon
        IMPORTING iv_trend        TYPE char1
        RETURNING VALUE(rv_icon)  TYPE char4,

      get_color_code
        IMPORTING iv_status       TYPE char1
        RETURNING VALUE(rv_color) TYPE char4,

      " Time calculation
      calculate_hours_diff
        IMPORTING iv_date1        TYPE sydatum
                  iv_time1        TYPE syuzeit
                  iv_date2        TYPE sydatum
                  iv_time2        TYPE syuzeit
        RETURNING VALUE(rv_hours) TYPE p LENGTH 7 DECIMALS 2,

      " Progress bar generation
      generate_bar_graph
        IMPORTING iv_value        TYPE numeric
                  iv_max          TYPE numeric
                  iv_width        TYPE i DEFAULT 30
        RETURNING VALUE(rv_bar)   TYPE string,

      " Percentage calculation
      calc_percentage
        IMPORTING iv_part         TYPE numeric
                  iv_total        TYPE numeric
        RETURNING VALUE(rv_pct)   TYPE p LENGTH 5 DECIMALS 2,

      " Day name
      get_day_name
        IMPORTING iv_date         TYPE sydatum
        RETURNING VALUE(rv_day)   TYPE char10.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_utilities IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_utilities IMPLEMENTATION.

  METHOD get_status_icon.
    rv_icon = SWITCH #( iv_status
      WHEN gc_status_green  THEN icon_led_green
      WHEN gc_status_yellow THEN icon_led_yellow
      WHEN gc_status_red    THEN icon_led_red
      ELSE icon_led_inactive ).
  ENDMETHOD.

  METHOD get_trend_icon.
    rv_icon = SWITCH #( iv_trend
      WHEN 'U' THEN icon_trend_up
      WHEN 'D' THEN icon_trend_down
      WHEN 'S' THEN icon_trend_stable
      ELSE icon_space ).
  ENDMETHOD.

  METHOD get_color_code.
    rv_color = SWITCH #( iv_status
      WHEN gc_status_green  THEN 'C510'  " Green background
      WHEN gc_status_yellow THEN 'C310'  " Yellow background
      WHEN gc_status_red    THEN 'C610'  " Red background
      ELSE 'C000' ).  " Default
  ENDMETHOD.

  METHOD calculate_hours_diff.
    DATA: lv_timestamp1 TYPE timestamp,
          lv_timestamp2 TYPE timestamp,
          lv_seconds    TYPE i.

    " Handle empty confirmation date (TO not yet confirmed)
    IF iv_date2 IS INITIAL.
      " Use current date/time
      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp2 TIME ZONE sy-zonlo.
    ELSE.
      CONVERT DATE iv_date2 TIME iv_time2 INTO TIME STAMP lv_timestamp2 TIME ZONE sy-zonlo.
    ENDIF.

    CONVERT DATE iv_date1 TIME iv_time1 INTO TIME STAMP lv_timestamp1 TIME ZONE sy-zonlo.

    lv_seconds = cl_abap_tstmp=>subtract(
      tstmp1 = lv_timestamp2
      tstmp2 = lv_timestamp1 ).

    rv_hours = lv_seconds / 3600.
  ENDMETHOD.

  METHOD generate_bar_graph.
    DATA: lv_filled   TYPE i,
          lv_empty    TYPE i,
          lv_pct      TYPE p LENGTH 5 DECIMALS 2.

    IF iv_max > 0.
      lv_pct = ( iv_value / iv_max ) * 100.
      lv_filled = iv_width * lv_pct / 100.
      IF lv_filled > iv_width.
        lv_filled = iv_width.
      ENDIF.
      lv_empty = iv_width - lv_filled.
    ENDIF.

    " Create ASCII progress bar
    rv_bar = |[{ repeat( val = '█' occ = lv_filled ) }{ repeat( val = '░' occ = lv_empty ) }] { lv_pct }%|.
  ENDMETHOD.

  METHOD calc_percentage.
    IF iv_total > 0.
      rv_pct = ( iv_part / iv_total ) * 100.
    ENDIF.
  ENDMETHOD.

  METHOD get_day_name.
    DATA: lv_day_num TYPE i.

    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = iv_date
      IMPORTING
        day  = lv_day_num.

    rv_day = SWITCH #( lv_day_num
      WHEN 1 THEN 'Monday'
      WHEN 2 THEN 'Tuesday'
      WHEN 3 THEN 'Wednesday'
      WHEN 4 THEN 'Thursday'
      WHEN 5 THEN 'Friday'
      WHEN 6 THEN 'Saturday'
      WHEN 7 THEN 'Sunday' ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_data_extractor DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_data_extractor DEFINITION FINAL.
  PUBLIC SECTION.
    " Range types using actual table field types
    TYPES: gty_r_lgnum TYPE RANGE OF lagp-lgnum,
           gty_r_lgtyp TYPE RANGE OF lagp-lgtyp,
           gty_r_lgpla TYPE RANGE OF lagp-lgpla,
           gty_r_matnr TYPE RANGE OF lqua-matnr,
           gty_r_bwlvs TYPE RANGE OF ltak-bwlvs.

    METHODS:
      constructor
        IMPORTING
          it_lgnum TYPE gty_r_lgnum
          it_lgtyp TYPE gty_r_lgtyp
          it_lgpla TYPE gty_r_lgpla
          it_matnr TYPE gty_r_matnr
          it_bwlvs TYPE gty_r_bwlvs
          iv_date_from TYPE sydatum
          iv_date_to   TYPE sydatum,

      extract_storage_bins
        RETURNING VALUE(rt_bins) TYPE gty_storage_bins,

      extract_transfer_orders
        RETURNING VALUE(rt_orders) TYPE gty_transfer_orders,

      get_storage_type_summary
        RETURNING VALUE(rt_summary) TYPE gty_storage_type_sums,

      get_movement_kpis
        RETURNING VALUE(rt_kpis) TYPE gty_movement_kpis,

      get_workload_analysis
        RETURNING VALUE(rt_workload) TYPE gty_workloads,

      get_user_workload
        RETURNING VALUE(rt_users) TYPE gty_user_workloads,

      get_daily_statistics
        RETURNING VALUE(rt_stats) TYPE gty_daily_stats,

      get_aging_analysis
        RETURNING VALUE(rt_aging) TYPE gty_agings,

      get_material_flow
        RETURNING VALUE(rt_flow) TYPE gty_material_flows,

      get_movement_simulation
        RETURNING VALUE(rt_sim) TYPE gty_movement_sims.

  PRIVATE SECTION.
    DATA:
      mt_lgnum     TYPE gty_r_lgnum,
      mt_lgtyp     TYPE gty_r_lgtyp,
      mt_lgpla     TYPE gty_r_lgpla,
      mt_matnr     TYPE gty_r_matnr,
      mt_bwlvs     TYPE gty_r_bwlvs,
      mv_date_from TYPE sydatum,
      mv_date_to   TYPE sydatum.

    METHODS:
      get_movement_type_text
        IMPORTING iv_bwlvs       TYPE ltak-bwlvs
        RETURNING VALUE(rv_text) TYPE char40,

      get_material_description
        IMPORTING iv_matnr       TYPE lqua-matnr
        RETURNING VALUE(rv_text) TYPE char40,

      get_storage_type_text
        IMPORTING iv_lgnum       TYPE lagp-lgnum
                  iv_lgtyp       TYPE lagp-lgtyp
        RETURNING VALUE(rv_text) TYPE char30.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_data_extractor IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_data_extractor IMPLEMENTATION.

  METHOD constructor.
    mt_lgnum     = it_lgnum.
    mt_lgtyp     = it_lgtyp.
    mt_lgpla     = it_lgpla.
    mt_matnr     = it_matnr.
    mt_bwlvs     = it_bwlvs.
    mv_date_from = iv_date_from.
    mv_date_to   = iv_date_to.
  ENDMETHOD.

  METHOD extract_storage_bins.
    DATA: lt_lagp  TYPE STANDARD TABLE OF lagp,
          lt_lqua  TYPE STANDARD TABLE OF lqua,
          ls_bin   TYPE gty_storage_bin.

    " Extract storage bins with all relevant fields
    SELECT lgnum, lgtyp, lgpla, lgber, lptyp, maxle, anzle,
           skzua, skzue, skzsa, skzse, skzsi
      FROM lagp
      INTO CORRESPONDING FIELDS OF TABLE @lt_lagp
      WHERE lgnum IN @mt_lgnum
        AND lgtyp IN @mt_lgtyp
        AND lgpla IN @mt_lgpla
      UP TO @gc_max_records ROWS.

    " Extract quants for quantity calculation
    IF lt_lagp IS NOT INITIAL.
      SELECT lgnum, lgtyp, lgpla, matnr, verme, gesme, meins
        FROM lqua
        INTO CORRESPONDING FIELDS OF TABLE @lt_lqua
        FOR ALL ENTRIES IN @lt_lagp
        WHERE lgnum = @lt_lagp-lgnum
          AND lgtyp = @lt_lagp-lgtyp
          AND lgpla = @lt_lagp-lgpla.
    ENDIF.

    " Process bins
    LOOP AT lt_lagp INTO DATA(ls_lagp).
      CLEAR ls_bin.
      ls_bin-lgnum  = ls_lagp-lgnum.
      ls_bin-lgtyp  = ls_lagp-lgtyp.
      ls_bin-lgpla  = ls_lagp-lgpla.
      ls_bin-lgber  = ls_lagp-lgber.
      ls_bin-lptyp  = ls_lagp-lptyp.
      ls_bin-maxle  = ls_lagp-maxle.
      ls_bin-anzle  = ls_lagp-anzle.

      " Check if bin is blocked (any block indicator set)
      IF ls_lagp-skzua IS NOT INITIAL OR
         ls_lagp-skzue IS NOT INITIAL OR
         ls_lagp-skzsa IS NOT INITIAL OR
         ls_lagp-skzse IS NOT INITIAL OR
         ls_lagp-skzsi IS NOT INITIAL.
        ls_bin-blocked = abap_true.
      ENDIF.

      " Calculate quant data for this bin
      LOOP AT lt_lqua INTO DATA(ls_lqua)
        WHERE lgnum = ls_lagp-lgnum
          AND lgtyp = ls_lagp-lgtyp
          AND lgpla = ls_lagp-lgpla.

        ls_bin-verme = ls_bin-verme + ls_lqua-verme.
        ls_bin-gesme = ls_bin-gesme + ls_lqua-gesme.
        ls_bin-quant_count = ls_bin-quant_count + 1.
        IF ls_bin-matnr IS INITIAL.
          ls_bin-matnr = ls_lqua-matnr.
          ls_bin-meins = ls_lqua-meins.
        ELSEIF ls_bin-matnr <> ls_lqua-matnr.
          ls_bin-mat_count = ls_bin-mat_count + 1.
        ENDIF.
      ENDLOOP.

      " Calculate occupancy based on MAXLE if available, otherwise on quant presence
      IF ls_bin-maxle > 0.
        ls_bin-occupancy = ( ls_bin-anzle / ls_bin-maxle ) * 100.
      ELSEIF ls_bin-quant_count > 0.
        ls_bin-occupancy = 100.  " Bin has quants but no max defined
      ELSE.
        ls_bin-occupancy = 0.    " Bin is empty
      ENDIF.

      " Determine status
      ls_bin-status = COND #(
        WHEN ls_bin-blocked = abap_true THEN gc_status_red
        WHEN ls_bin-occupancy >= gc_occupancy_high THEN gc_status_yellow
        WHEN ls_bin-occupancy > 0 THEN gc_status_green
        ELSE gc_status_green ).

      ls_bin-status_icon = lcl_utilities=>get_status_icon( ls_bin-status ).
      ls_bin-color = lcl_utilities=>get_color_code( ls_bin-status ).

      APPEND ls_bin TO rt_bins.
    ENDLOOP.

    " Sort by warehouse, type, bin
    SORT rt_bins BY lgnum lgtyp lgpla.
  ENDMETHOD.

  METHOD extract_transfer_orders.
    DATA: ls_to TYPE gty_transfer_order.

    " Define work area structure for join result
    TYPES: BEGIN OF lty_to_data,
             " From LTAK (header)
             lgnum TYPE ltak-lgnum,
             tanum TYPE ltak-tanum,
             bwlvs TYPE ltak-bwlvs,
             refnr TYPE ltak-refnr,
             bdatu TYPE ltak-bdatu,
             bzeit TYPE ltak-bzeit,
             " From LTAP (item)
             tapos TYPE ltap-tapos,
             vltyp TYPE ltap-vltyp,
             vlpla TYPE ltap-vlpla,
             nltyp TYPE ltap-nltyp,
             nlpla TYPE ltap-nlpla,
             matnr TYPE ltap-matnr,
             werks TYPE ltap-werks,
             maktx TYPE ltap-maktx,
             vsolm TYPE ltap-vsolm,
             meins TYPE ltap-meins,
             qdatu TYPE ltap-qdatu,
             qzeit TYPE ltap-qzeit,
             qname TYPE ltap-qname,
           END OF lty_to_data.

    DATA: lt_to_data TYPE STANDARD TABLE OF lty_to_data.

    " Join LTAK (header) and LTAP (items) to get all required fields
    " BWLVS, BDATU, BZEIT are in LTAK; confirmation fields QDATU, QZEIT, QNAME are in LTAP
    SELECT ltak~lgnum, ltak~tanum, ltak~bwlvs, ltak~refnr, ltak~bdatu, ltak~bzeit,
           ltap~tapos, ltap~vltyp, ltap~vlpla, ltap~nltyp, ltap~nlpla,
           ltap~matnr, ltap~werks, ltap~maktx, ltap~vsolm, ltap~meins,
           ltap~qdatu, ltap~qzeit, ltap~qname
      FROM ltak
      INNER JOIN ltap ON ltak~lgnum = ltap~lgnum AND ltak~tanum = ltap~tanum
      INTO TABLE @lt_to_data
      WHERE ltak~lgnum IN @mt_lgnum
        AND ltak~bwlvs IN @mt_bwlvs
        AND ltap~matnr IN @mt_matnr
        AND ltak~bdatu BETWEEN @mv_date_from AND @mv_date_to
      UP TO @gc_max_records ROWS.

    " Process TOs
    LOOP AT lt_to_data INTO DATA(ls_data).
      CLEAR ls_to.

      " Header fields from LTAK
      ls_to-lgnum = ls_data-lgnum.
      ls_to-tanum = ls_data-tanum.
      ls_to-bwlvs = ls_data-bwlvs.
      ls_to-refnr = ls_data-refnr.
      ls_to-bdatu = ls_data-bdatu.
      ls_to-bzeit = ls_data-bzeit.

      " Item fields from LTAP
      ls_to-tapos = ls_data-tapos.
      ls_to-vltyp = ls_data-vltyp.
      ls_to-vlpla = ls_data-vlpla.
      ls_to-nltyp = ls_data-nltyp.
      ls_to-nlpla = ls_data-nlpla.
      ls_to-matnr = ls_data-matnr.
      ls_to-werks = ls_data-werks.
      ls_to-vsolm = ls_data-vsolm.
      ls_to-meins = ls_data-meins.

      " Confirmation fields from LTAP
      ls_to-qdatu = ls_data-qdatu.
      ls_to-qzeit = ls_data-qzeit.
      ls_to-qname = ls_data-qname.

      " Get material description (use from LTAP if available, otherwise fetch)
      IF ls_data-maktx IS NOT INITIAL.
        ls_to-maktx = ls_data-maktx.
      ELSE.
        ls_to-maktx = get_material_description( ls_data-matnr ).
      ENDIF.

      " Determine if confirmed (QDATU is confirmation date)
      ls_to-confirmed = COND #( WHEN ls_data-qdatu IS NOT INITIAL THEN abap_true ).

      " Calculate waiting time
      ls_to-wait_hours = lcl_utilities=>calculate_hours_diff(
        iv_date1 = ls_data-bdatu
        iv_time1 = ls_data-bzeit
        iv_date2 = ls_data-qdatu
        iv_time2 = ls_data-qzeit ).

      " Set status
      IF ls_to-confirmed = abap_true.
        ls_to-status = 'Confirmed'.
        ls_to-status_icon = icon_okay.
        ls_to-color = 'C510'.
      ELSE.
        IF ls_to-wait_hours > gc_to_time_warn.
          ls_to-status = 'Critical'.
          ls_to-status_icon = icon_led_red.
          ls_to-color = 'C610'.
        ELSEIF ls_to-wait_hours > gc_to_time_good.
          ls_to-status = 'Warning'.
          ls_to-status_icon = icon_led_yellow.
          ls_to-color = 'C310'.
        ELSE.
          ls_to-status = 'Open'.
          ls_to-status_icon = icon_led_green.
          ls_to-color = 'C000'.
        ENDIF.
      ENDIF.

      APPEND ls_to TO rt_orders.
    ENDLOOP.

    " Sort by date/time descending (newest first)
    SORT rt_orders BY bdatu DESCENDING bzeit DESCENDING lgnum tanum tapos.
  ENDMETHOD.

  METHOD get_storage_type_summary.
    DATA: lt_bins TYPE gty_storage_bins,
          ls_sum  TYPE gty_storage_type_sum.

    lt_bins = extract_storage_bins( ).

    " Group by warehouse and storage type
    LOOP AT lt_bins INTO DATA(ls_bin)
      GROUP BY ( lgnum = ls_bin-lgnum
                 lgtyp = ls_bin-lgtyp )
      INTO DATA(lo_group).

      CLEAR ls_sum.
      ls_sum-lgnum = lo_group-lgnum.
      ls_sum-lgtyp = lo_group-lgtyp.
      ls_sum-lgtyp_txt = get_storage_type_text( iv_lgnum = lo_group-lgnum
                                                 iv_lgtyp = lo_group-lgtyp ).

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        ls_sum-total_bins = ls_sum-total_bins + 1.
        ls_sum-total_quants = ls_sum-total_quants + ls_member-quant_count.

        IF ls_member-blocked = abap_true.
          ls_sum-blocked_bins = ls_sum-blocked_bins + 1.
        ELSEIF ls_member-quant_count > 0.
          ls_sum-occupied_bins = ls_sum-occupied_bins + 1.
        ELSE.
          ls_sum-empty_bins = ls_sum-empty_bins + 1.
        ENDIF.
      ENDLOOP.

      " Calculate occupancy percentage
      IF ls_sum-total_bins > 0.
        ls_sum-occupancy_pct = ( ls_sum-occupied_bins / ls_sum-total_bins ) * 100.
      ENDIF.

      " Generate bar graph
      ls_sum-bar_graph = lcl_utilities=>generate_bar_graph(
        iv_value = ls_sum-occupied_bins
        iv_max   = ls_sum-total_bins
        iv_width = 20 ).

      " Set status
      ls_sum-status = COND #(
        WHEN ls_sum-occupancy_pct >= gc_occupancy_high THEN gc_status_yellow
        WHEN ls_sum-blocked_bins > 0 THEN gc_status_yellow
        ELSE gc_status_green ).

      ls_sum-status_icon = lcl_utilities=>get_status_icon( ls_sum-status ).

      APPEND ls_sum TO rt_summary.
    ENDLOOP.

    SORT rt_summary BY lgnum lgtyp.
  ENDMETHOD.

  METHOD get_movement_kpis.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_kpi    TYPE gty_movement_kpi.

    lt_orders = extract_transfer_orders( ).

    " Group by warehouse and movement type
    LOOP AT lt_orders INTO DATA(ls_order)
      GROUP BY ( lgnum = ls_order-lgnum
                 bwlvs = ls_order-bwlvs )
      INTO DATA(lo_group).

      CLEAR ls_kpi.
      ls_kpi-lgnum = lo_group-lgnum.
      ls_kpi-bwlvs = lo_group-bwlvs.
      ls_kpi-bwlvs_txt = get_movement_type_text( lo_group-bwlvs ).

      DATA: lv_total_hours TYPE p LENGTH 15 DECIMALS 2,
            lv_min_hours   TYPE p LENGTH 7 DECIMALS 2 VALUE 99999,
            lv_max_hours   TYPE p LENGTH 7 DECIMALS 2 VALUE 0.

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        ls_kpi-to_count = ls_kpi-to_count + 1.
        ls_kpi-total_qty = ls_kpi-total_qty + ls_member-vsolm.

        IF ls_member-confirmed = abap_true.
          ls_kpi-to_confirmed = ls_kpi-to_confirmed + 1.
          lv_total_hours = lv_total_hours + ls_member-wait_hours.

          IF ls_member-wait_hours < lv_min_hours.
            lv_min_hours = ls_member-wait_hours.
          ENDIF.
          IF ls_member-wait_hours > lv_max_hours.
            lv_max_hours = ls_member-wait_hours.
          ENDIF.
        ELSE.
          ls_kpi-to_open = ls_kpi-to_open + 1.
        ENDIF.
      ENDLOOP.

      " Calculate averages
      IF ls_kpi-to_confirmed > 0.
        ls_kpi-avg_time_hours = lv_total_hours / ls_kpi-to_confirmed.
        ls_kpi-min_time_hours = lv_min_hours.
        ls_kpi-max_time_hours = lv_max_hours.
      ENDIF.

      " Set status based on open TOs and avg time
      ls_kpi-status = COND #(
        WHEN ls_kpi-to_open > ( ls_kpi-to_count / 2 ) THEN gc_status_red
        WHEN ls_kpi-avg_time_hours > gc_to_time_warn THEN gc_status_yellow
        ELSE gc_status_green ).

      ls_kpi-status_icon = lcl_utilities=>get_status_icon( ls_kpi-status ).

      APPEND ls_kpi TO rt_kpis.
    ENDLOOP.

    SORT rt_kpis BY lgnum bwlvs.
  ENDMETHOD.

  METHOD get_workload_analysis.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_wl     TYPE gty_workload.

    lt_orders = extract_transfer_orders( ).

    " Group by warehouse, date, and hour
    LOOP AT lt_orders INTO DATA(ls_order)
      GROUP BY ( lgnum = ls_order-lgnum
                 date  = ls_order-bdatu
                 hour  = CONV i( ls_order-bzeit+0(2) ) )
      INTO DATA(lo_group).

      CLEAR ls_wl.
      ls_wl-lgnum = lo_group-lgnum.
      ls_wl-date  = lo_group-date.
      ls_wl-hour  = lo_group-hour.

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        ls_wl-to_created = ls_wl-to_created + 1.
        ls_wl-items_moved = ls_wl-items_moved + ls_member-vsolm.

        IF ls_member-confirmed = abap_true.
          ls_wl-to_confirmed = ls_wl-to_confirmed + 1.
        ELSE.
          ls_wl-to_open = ls_wl-to_open + 1.
        ENDIF.
      ENDLOOP.

      APPEND ls_wl TO rt_workload.
    ENDLOOP.

    " Mark peak hours
    DATA(lv_max_created) = REDUCE i(
      INIT max = 0
      FOR wa IN rt_workload
      NEXT max = COND #( WHEN wa-to_created > max THEN wa-to_created ELSE max ) ).

    LOOP AT rt_workload ASSIGNING FIELD-SYMBOL(<fs_wl>).
      IF <fs_wl>-to_created = lv_max_created.
        <fs_wl>-peak_flag = abap_true.
      ENDIF.
    ENDLOOP.

    SORT rt_workload BY lgnum date hour.
  ENDMETHOD.

  METHOD get_user_workload.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_user   TYPE gty_user_workload.

    lt_orders = extract_transfer_orders( ).

    " Filter only confirmed orders with user
    DELETE lt_orders WHERE confirmed <> abap_true.
    DELETE lt_orders WHERE qname IS INITIAL.

    " Group by warehouse and user
    LOOP AT lt_orders INTO DATA(ls_order)
      GROUP BY ( lgnum = ls_order-lgnum
                 qname = ls_order-qname )
      INTO DATA(lo_group).

      CLEAR ls_user.
      ls_user-lgnum = lo_group-lgnum.
      ls_user-qname = lo_group-qname.

      DATA: lv_total_hours TYPE p LENGTH 15 DECIMALS 2.

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        ls_user-to_confirmed = ls_user-to_confirmed + 1.
        ls_user-items_processed = ls_user-items_processed + ls_member-vsolm.
        lv_total_hours = lv_total_hours + ls_member-wait_hours.
      ENDLOOP.

      IF ls_user-to_confirmed > 0.
        ls_user-avg_time_hours = lv_total_hours / ls_user-to_confirmed.
      ENDIF.

      APPEND ls_user TO rt_users.
    ENDLOOP.

    " Calculate efficiency (relative to best performer)
    IF rt_users IS NOT INITIAL.
      DATA(lv_min_avg) = REDUCE p(
        INIT min TYPE p LENGTH 7 DECIMALS 2 VALUE 99999
        FOR wa IN rt_users
        NEXT min = COND #( WHEN wa-avg_time_hours < min AND wa-avg_time_hours > 0
                           THEN wa-avg_time_hours ELSE min ) ).

      LOOP AT rt_users ASSIGNING FIELD-SYMBOL(<fs_user>).
        IF <fs_user>-avg_time_hours > 0 AND lv_min_avg > 0.
          <fs_user>-efficiency = ( lv_min_avg / <fs_user>-avg_time_hours ) * 100.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT rt_users BY lgnum to_confirmed DESCENDING.
  ENDMETHOD.

  METHOD get_daily_statistics.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_stat   TYPE gty_daily_stat.

    lt_orders = extract_transfer_orders( ).

    " Group by warehouse and date
    LOOP AT lt_orders INTO DATA(ls_order)
      GROUP BY ( lgnum = ls_order-lgnum
                 date  = ls_order-bdatu )
      INTO DATA(lo_group).

      CLEAR ls_stat.
      ls_stat-lgnum = lo_group-lgnum.
      ls_stat-date  = lo_group-date.
      ls_stat-day_name = lcl_utilities=>get_day_name( lo_group-date ).

      DATA: lv_total_hours TYPE p LENGTH 15 DECIMALS 2,
            lt_hours       TYPE STANDARD TABLE OF i,
            lv_confirmed   TYPE i.

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        ls_stat-to_created = ls_stat-to_created + 1.
        ls_stat-total_qty = ls_stat-total_qty + ls_member-vsolm.

        IF ls_member-confirmed = abap_true.
          ls_stat-to_confirmed = ls_stat-to_confirmed + 1.
          lv_total_hours = lv_total_hours + ls_member-wait_hours.
          lv_confirmed = lv_confirmed + 1.
        ENDIF.

        " Track hours for peak calculation
        APPEND CONV i( ls_member-bzeit+0(2) ) TO lt_hours.
      ENDLOOP.

      IF lv_confirmed > 0.
        ls_stat-avg_confirm_hrs = lv_total_hours / lv_confirmed.
      ENDIF.

      " Find peak hour (most frequent)
      IF lt_hours IS NOT INITIAL.
        SORT lt_hours.
        DATA: lv_prev_hour  TYPE i VALUE -1,
              lv_count      TYPE i,
              lv_max_count  TYPE i,
              lv_peak_hour  TYPE i.

        LOOP AT lt_hours INTO DATA(lv_hour).
          IF lv_hour = lv_prev_hour.
            lv_count = lv_count + 1.
          ELSE.
            IF lv_count > lv_max_count.
              lv_max_count = lv_count.
              lv_peak_hour = lv_prev_hour.
            ENDIF.
            lv_count = 1.
            lv_prev_hour = lv_hour.
          ENDIF.
        ENDLOOP.
        IF lv_count > lv_max_count.
          lv_peak_hour = lv_prev_hour.
        ENDIF.
        ls_stat-peak_hour = lv_peak_hour.
      ENDIF.

      APPEND ls_stat TO rt_stats.
    ENDLOOP.

    " Generate bar graphs based on max values
    IF rt_stats IS NOT INITIAL.
      DATA(lv_max_created) = REDUCE i(
        INIT max = 0
        FOR wa IN rt_stats
        NEXT max = COND #( WHEN wa-to_created > max THEN wa-to_created ELSE max ) ).

      LOOP AT rt_stats ASSIGNING FIELD-SYMBOL(<fs_stat>).
        <fs_stat>-bar_created = lcl_utilities=>generate_bar_graph(
          iv_value = <fs_stat>-to_created
          iv_max   = lv_max_created
          iv_width = 15 ).
        <fs_stat>-bar_confirmed = lcl_utilities=>generate_bar_graph(
          iv_value = <fs_stat>-to_confirmed
          iv_max   = lv_max_created
          iv_width = 15 ).
      ENDLOOP.
    ENDIF.

    SORT rt_stats BY lgnum date.
  ENDMETHOD.

  METHOD get_aging_analysis.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_aging  TYPE gty_aging.

    lt_orders = extract_transfer_orders( ).

    " Filter only open orders
    DELETE lt_orders WHERE confirmed = abap_true.

    " Define aging buckets
    DATA: lt_buckets TYPE STANDARD TABLE OF i,
          lt_bucket_names TYPE STANDARD TABLE OF char20.

    lt_buckets = VALUE #( ( 4 ) ( 8 ) ( 24 ) ( 48 ) ( 999999 ) ).
    lt_bucket_names = VALUE #( ( '< 4 hours' ) ( '4-8 hours' ) ( '8-24 hours' ) ( '1-2 days' ) ( '> 2 days' ) ).

    " Count by warehouse and bucket
    DATA: lt_temp TYPE STANDARD TABLE OF gty_aging.

    LOOP AT lt_orders INTO DATA(ls_order)
      GROUP BY ( lgnum = ls_order-lgnum )
      INTO DATA(lo_group).

      DATA: lv_bucket_idx TYPE i.

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        " Determine bucket
        lv_bucket_idx = 1.
        LOOP AT lt_buckets INTO DATA(lv_bucket).
          IF ls_member-wait_hours <= lv_bucket.
            EXIT.
          ENDIF.
          lv_bucket_idx = lv_bucket_idx + 1.
        ENDLOOP.

        " Find or create aging record
        READ TABLE lt_temp ASSIGNING FIELD-SYMBOL(<fs_temp>)
          WITH KEY lgnum = lo_group-lgnum
                   age_bucket = lt_bucket_names[ lv_bucket_idx ].
        IF sy-subrc <> 0.
          ls_aging-lgnum = lo_group-lgnum.
          ls_aging-age_bucket = lt_bucket_names[ lv_bucket_idx ].
          ls_aging-to_count = 1.
          APPEND ls_aging TO lt_temp.
        ELSE.
          <fs_temp>-to_count = <fs_temp>-to_count + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " Calculate percentages and bar graphs
    LOOP AT lt_temp INTO DATA(ls_temp)
      GROUP BY ( lgnum = ls_temp-lgnum )
      INTO DATA(lo_grp).

      DATA(lv_total) = REDUCE i(
        INIT sum = 0
        FOR wa IN GROUP lo_grp
        NEXT sum = sum + wa-to_count ).

      LOOP AT GROUP lo_grp ASSIGNING FIELD-SYMBOL(<fs_grp>).
        <fs_grp>-percentage = lcl_utilities=>calc_percentage(
          iv_part  = <fs_grp>-to_count
          iv_total = lv_total ).
        <fs_grp>-bar_graph = lcl_utilities=>generate_bar_graph(
          iv_value = <fs_grp>-to_count
          iv_max   = lv_total
          iv_width = 20 ).
        APPEND <fs_grp> TO rt_aging.
      ENDLOOP.
    ENDLOOP.

    SORT rt_aging BY lgnum age_bucket.
  ENDMETHOD.

  METHOD get_material_flow.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_flow   TYPE gty_material_flow.

    lt_orders = extract_transfer_orders( ).

    " Group by warehouse and material
    LOOP AT lt_orders INTO DATA(ls_order)
      GROUP BY ( lgnum = ls_order-lgnum
                 matnr = ls_order-matnr )
      INTO DATA(lo_group).

      CLEAR ls_flow.
      ls_flow-lgnum = lo_group-lgnum.
      ls_flow-matnr = lo_group-matnr.
      ls_flow-maktx = get_material_description( lo_group-matnr ).

      LOOP AT GROUP lo_group INTO DATA(ls_member).
        ls_flow-movements = ls_flow-movements + 1.
        ls_flow-total_qty = ls_flow-total_qty + ls_member-vsolm.
        ls_flow-meins = ls_member-meins.

        " Classify movement direction
        " Movement types starting with 9 are usually internal
        " Types starting with 1,2,3 vary by configuration
        " Using source/destination to determine direction
        IF ls_member-vltyp IS INITIAL OR ls_member-vltyp = '   '.
          " No source = incoming
          ls_flow-inbound = ls_flow-inbound + 1.
        ELSEIF ls_member-nltyp IS INITIAL OR ls_member-nltyp = '   '.
          " No destination = outgoing
          ls_flow-outbound = ls_flow-outbound + 1.
        ELSE.
          " Both have values = internal
          ls_flow-internal = ls_flow-internal + 1.
        ENDIF.
      ENDLOOP.

      APPEND ls_flow TO rt_flow.
    ENDLOOP.

    " Sort by movements descending
    SORT rt_flow BY lgnum movements DESCENDING.
  ENDMETHOD.

  METHOD get_movement_simulation.
    DATA: lt_orders TYPE gty_transfer_orders,
          ls_sim    TYPE gty_movement_sim.

    lt_orders = extract_transfer_orders( ).

    LOOP AT lt_orders INTO DATA(ls_order).
      CLEAR ls_sim.
      ls_sim-lgnum = ls_order-lgnum.
      ls_sim-tanum = ls_order-tanum.
      ls_sim-tapos = ls_order-tapos.
      ls_sim-matnr = ls_order-matnr.
      ls_sim-maktx = ls_order-maktx.
      ls_sim-vsolm = ls_order-vsolm.
      ls_sim-meins = ls_order-meins.
      ls_sim-vltyp = ls_order-vltyp.
      ls_sim-vlpla = ls_order-vlpla.
      ls_sim-nltyp = ls_order-nltyp.
      ls_sim-nlpla = ls_order-nlpla.
      ls_sim-bdatu = ls_order-bdatu.
      ls_sim-bzeit = ls_order-bzeit.

      " Create timestamp for sorting
      CONVERT DATE ls_order-bdatu TIME ls_order-bzeit
        INTO TIME STAMP ls_sim-timestamp TIME ZONE sy-zonlo.

      " Create direction indicator
      ls_sim-direction = |{ ls_sim-vltyp }/{ ls_sim-vlpla } → { ls_sim-nltyp }/{ ls_sim-nlpla }|.

      APPEND ls_sim TO rt_sim.
    ENDLOOP.

    " Sort by timestamp
    SORT rt_sim BY timestamp lgnum tanum tapos.
  ENDMETHOD.

  METHOD get_movement_type_text.
    SELECT SINGLE btext
      FROM t333t
      INTO @rv_text
      WHERE spras = @sy-langu
        AND bwlvs = @iv_bwlvs.
    IF sy-subrc <> 0.
      rv_text = iv_bwlvs.
    ENDIF.
  ENDMETHOD.

  METHOD get_material_description.
    SELECT SINGLE maktx
      FROM makt
      INTO @rv_text
      WHERE matnr = @iv_matnr
        AND spras = @sy-langu.
  ENDMETHOD.

  METHOD get_storage_type_text.
    SELECT SINGLE ltypt
      FROM t301t
      INTO @rv_text
      WHERE lgnum = @iv_lgnum
        AND lgtyp = @iv_lgtyp
        AND spras = @sy-langu.
    IF sy-subrc <> 0.
      rv_text = iv_lgtyp.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_html_dashboard DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_dashboard DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      generate_overview_html
        IMPORTING
          iv_total_bins       TYPE i
          iv_occupied_bins    TYPE i
          iv_empty_bins       TYPE i
          iv_blocked_bins     TYPE i
          iv_total_to         TYPE i
          iv_open_to          TYPE i
          iv_confirmed_to     TYPE i
          iv_avg_confirm_time TYPE p
          iv_occupancy_pct    TYPE p
        RETURNING
          VALUE(rv_html) TYPE string,

      generate_kpi_card
        IMPORTING
          iv_title       TYPE string
          iv_value       TYPE string
          iv_unit        TYPE string OPTIONAL
          iv_color       TYPE string DEFAULT '#2196F3'
          iv_icon        TYPE string OPTIONAL
        RETURNING
          VALUE(rv_html) TYPE string,

      generate_progress_bar
        IMPORTING
          iv_value       TYPE numeric
          iv_max         TYPE numeric
          iv_color       TYPE string DEFAULT '#4CAF50'
          iv_height      TYPE i DEFAULT 20
        RETURNING
          VALUE(rv_html) TYPE string,

      generate_chart_container
        IMPORTING
          iv_title       TYPE string
          iv_content     TYPE string
        RETURNING
          VALUE(rv_html) TYPE string,

      get_css_styles
        RETURNING
          VALUE(rv_css) TYPE string.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_html_dashboard IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_dashboard IMPLEMENTATION.

  METHOD generate_overview_html.
    DATA: lv_bins_pct   TYPE p LENGTH 5 DECIMALS 1,
          lv_to_pct     TYPE p LENGTH 5 DECIMALS 1.

    IF iv_total_bins > 0.
      lv_bins_pct = ( iv_occupied_bins / iv_total_bins ) * 100.
    ENDIF.
    IF iv_total_to > 0.
      lv_to_pct = ( iv_confirmed_to / iv_total_to ) * 100.
    ENDIF.

    " Build HTML dashboard
    rv_html =
      |<html>| &&
      |<head>| &&
      |<style>{ get_css_styles( ) }</style>| &&
      |</head>| &&
      |<body>| &&
      |<div class="dashboard">| &&

      " Header
      |<div class="header">| &&
      |<h1>SAP WM Monitoring Dashboard</h1>| &&
      |<p class="subtitle">Real-time Warehouse Overview</p>| &&
      |</div>| &&

      " KPI Cards Row
      |<div class="kpi-row">| &&
      generate_kpi_card(
        iv_title = 'Total Bins'
        iv_value = |{ iv_total_bins }|
        iv_color = '#2196F3'
        iv_icon  = '📦' ) &&
      generate_kpi_card(
        iv_title = 'Occupied Bins'
        iv_value = |{ iv_occupied_bins }|
        iv_color = '#4CAF50'
        iv_icon  = '✓' ) &&
      generate_kpi_card(
        iv_title = 'Empty Bins'
        iv_value = |{ iv_empty_bins }|
        iv_color = '#9E9E9E'
        iv_icon  = '○' ) &&
      generate_kpi_card(
        iv_title = 'Blocked Bins'
        iv_value = |{ iv_blocked_bins }|
        iv_color = '#f44336'
        iv_icon  = '⊘' ) &&
      |</div>| &&

      " Transfer Order KPIs
      |<div class="kpi-row">| &&
      generate_kpi_card(
        iv_title = 'Total TOs'
        iv_value = |{ iv_total_to }|
        iv_color = '#673AB7'
        iv_icon  = '📋' ) &&
      generate_kpi_card(
        iv_title = 'Open TOs'
        iv_value = |{ iv_open_to }|
        iv_color = '#FF9800'
        iv_icon  = '⏳' ) &&
      generate_kpi_card(
        iv_title = 'Confirmed TOs'
        iv_value = |{ iv_confirmed_to }|
        iv_color = '#4CAF50'
        iv_icon  = '✓' ) &&
      generate_kpi_card(
        iv_title = 'Avg Confirm Time'
        iv_value = |{ iv_avg_confirm_time DECIMALS = 1 }|
        iv_unit  = 'hours'
        iv_color = '#00BCD4'
        iv_icon  = '⏱' ) &&
      |</div>| &&

      " Progress bars
      |<div class="charts-row">| &&
      generate_chart_container(
        iv_title   = 'Bin Occupancy'
        iv_content = generate_progress_bar(
                       iv_value = iv_occupied_bins
                       iv_max   = iv_total_bins
                       iv_color = COND #(
                         WHEN lv_bins_pct >= 85 THEN '#f44336'
                         WHEN lv_bins_pct >= 60 THEN '#FF9800'
                         ELSE '#4CAF50' ) ) ) &&
      generate_chart_container(
        iv_title   = 'TO Completion Rate'
        iv_content = generate_progress_bar(
                       iv_value = iv_confirmed_to
                       iv_max   = iv_total_to
                       iv_color = '#4CAF50' ) ) &&
      |</div>| &&

      " Status indicators
      |<div class="status-row">| &&
      |<div class="status-card">| &&
      |<div class="status-indicator { COND #(
         WHEN iv_open_to > ( iv_total_to / 2 ) THEN 'status-red'
         WHEN iv_open_to > ( iv_total_to / 4 ) THEN 'status-yellow'
         ELSE 'status-green' ) }"></div>| &&
      |<span>Workload Status: { COND string(
         WHEN iv_open_to > ( iv_total_to / 2 ) THEN 'High Backlog'
         WHEN iv_open_to > ( iv_total_to / 4 ) THEN 'Moderate'
         ELSE 'Normal' ) }</span>| &&
      |</div>| &&
      |<div class="status-card">| &&
      |<div class="status-indicator { COND #(
         WHEN iv_avg_confirm_time > 8 THEN 'status-red'
         WHEN iv_avg_confirm_time > 4 THEN 'status-yellow'
         ELSE 'status-green' ) }"></div>| &&
      |<span>Processing Speed: { COND string(
         WHEN iv_avg_confirm_time > 8 THEN 'Slow'
         WHEN iv_avg_confirm_time > 4 THEN 'Average'
         ELSE 'Good' ) }</span>| &&
      |</div>| &&
      |</div>| &&

      |</div>| &&
      |</body>| &&
      |</html>|.
  ENDMETHOD.

  METHOD generate_kpi_card.
    rv_html =
      |<div class="kpi-card" style="border-left: 4px solid { iv_color };">| &&
      |<div class="kpi-icon">{ iv_icon }</div>| &&
      |<div class="kpi-content">| &&
      |<div class="kpi-value" style="color: { iv_color };">{ iv_value }| &&
      COND #( WHEN iv_unit IS NOT INITIAL THEN | <span class="kpi-unit">{ iv_unit }</span>| ) &&
      |</div>| &&
      |<div class="kpi-title">{ iv_title }</div>| &&
      |</div>| &&
      |</div>|.
  ENDMETHOD.

  METHOD generate_progress_bar.
    DATA: lv_pct TYPE p LENGTH 5 DECIMALS 1.

    IF iv_max > 0.
      lv_pct = ( iv_value / iv_max ) * 100.
    ENDIF.

    rv_html =
      |<div class="progress-container">| &&
      |<div class="progress-bar" style="width: { lv_pct }%; background-color: { iv_color }; height: { iv_height }px;"></div>| &&
      |<span class="progress-text">{ lv_pct DECIMALS = 1 }%</span>| &&
      |</div>|.
  ENDMETHOD.

  METHOD generate_chart_container.
    rv_html =
      |<div class="chart-container">| &&
      |<h3>{ iv_title }</h3>| &&
      |{ iv_content }| &&
      |</div>|.
  ENDMETHOD.

  METHOD get_css_styles.
    rv_css =
      |body \{| &&
      |  font-family: 'Segoe UI', Arial, sans-serif;| &&
      |  margin: 0; padding: 10px;| &&
      |  background-color: #f5f5f5;| &&
      |\}| &&
      |.dashboard \{| &&
      |  max-width: 1200px;| &&
      |  margin: 0 auto;| &&
      |\}| &&
      |.header \{| &&
      |  text-align: center;| &&
      |  padding: 15px;| &&
      |  background: linear-gradient(135deg, #1976D2, #2196F3);| &&
      |  color: white;| &&
      |  border-radius: 8px;| &&
      |  margin-bottom: 15px;| &&
      |  box-shadow: 0 2px 4px rgba(0,0,0,0.1);| &&
      |\}| &&
      |.header h1 \{| &&
      |  margin: 0;| &&
      |  font-size: 24px;| &&
      |\}| &&
      |.subtitle \{| &&
      |  margin: 5px 0 0 0;| &&
      |  opacity: 0.9;| &&
      |  font-size: 14px;| &&
      |\}| &&
      |.kpi-row \{| &&
      |  display: flex;| &&
      |  gap: 15px;| &&
      |  margin-bottom: 15px;| &&
      |  flex-wrap: wrap;| &&
      |\}| &&
      |.kpi-card \{| &&
      |  flex: 1;| &&
      |  min-width: 150px;| &&
      |  background: white;| &&
      |  border-radius: 8px;| &&
      |  padding: 15px;| &&
      |  display: flex;| &&
      |  align-items: center;| &&
      |  box-shadow: 0 2px 4px rgba(0,0,0,0.1);| &&
      |\}| &&
      |.kpi-icon \{| &&
      |  font-size: 28px;| &&
      |  margin-right: 12px;| &&
      |\}| &&
      |.kpi-value \{| &&
      |  font-size: 28px;| &&
      |  font-weight: bold;| &&
      |\}| &&
      |.kpi-unit \{| &&
      |  font-size: 14px;| &&
      |  font-weight: normal;| &&
      |\}| &&
      |.kpi-title \{| &&
      |  font-size: 12px;| &&
      |  color: #666;| &&
      |  text-transform: uppercase;| &&
      |\}| &&
      |.charts-row \{| &&
      |  display: flex;| &&
      |  gap: 15px;| &&
      |  margin-bottom: 15px;| &&
      |\}| &&
      |.chart-container \{| &&
      |  flex: 1;| &&
      |  background: white;| &&
      |  border-radius: 8px;| &&
      |  padding: 15px;| &&
      |  box-shadow: 0 2px 4px rgba(0,0,0,0.1);| &&
      |\}| &&
      |.chart-container h3 \{| &&
      |  margin: 0 0 10px 0;| &&
      |  font-size: 14px;| &&
      |  color: #333;| &&
      |\}| &&
      |.progress-container \{| &&
      |  background: #e0e0e0;| &&
      |  border-radius: 10px;| &&
      |  overflow: hidden;| &&
      |  position: relative;| &&
      |\}| &&
      |.progress-bar \{| &&
      |  transition: width 0.3s ease;| &&
      |  border-radius: 10px;| &&
      |\}| &&
      |.progress-text \{| &&
      |  position: absolute;| &&
      |  right: 10px;| &&
      |  top: 50%;| &&
      |  transform: translateY(-50%);| &&
      |  font-weight: bold;| &&
      |  font-size: 12px;| &&
      |\}| &&
      |.status-row \{| &&
      |  display: flex;| &&
      |  gap: 15px;| &&
      |\}| &&
      |.status-card \{| &&
      |  flex: 1;| &&
      |  background: white;| &&
      |  border-radius: 8px;| &&
      |  padding: 15px;| &&
      |  display: flex;| &&
      |  align-items: center;| &&
      |  box-shadow: 0 2px 4px rgba(0,0,0,0.1);| &&
      |\}| &&
      |.status-indicator \{| &&
      |  width: 16px;| &&
      |  height: 16px;| &&
      |  border-radius: 50%;| &&
      |  margin-right: 10px;| &&
      |\}| &&
      |.status-green \{ background-color: #4CAF50; \}| &&
      |.status-yellow \{ background-color: #FF9800; \}| &&
      |.status-red \{ background-color: #f44336; \}|.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_alv_handler DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv_handler DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      go_instance TYPE REF TO lcl_alv_handler.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_alv_handler.

    METHODS:
      display_storage_bins
        IMPORTING it_data TYPE gty_storage_bins,

      display_transfer_orders
        IMPORTING it_data TYPE gty_transfer_orders,

      display_storage_type_summary
        IMPORTING it_data TYPE gty_storage_type_sums,

      display_movement_kpis
        IMPORTING it_data TYPE gty_movement_kpis,

      display_daily_statistics
        IMPORTING it_data TYPE gty_daily_stats,

      display_aging_analysis
        IMPORTING it_data TYPE gty_agings,

      display_user_workload
        IMPORTING it_data TYPE gty_user_workloads,

      display_material_flow
        IMPORTING it_data TYPE gty_material_flows,

      display_movement_simulation
        IMPORTING it_data TYPE gty_movement_sims,

      on_user_command
        FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

  PRIVATE SECTION.
    DATA:
      mo_salv TYPE REF TO cl_salv_table.

    METHODS:
      set_column_settings
        IMPORTING io_columns TYPE REF TO cl_salv_columns_table,

      set_color_column
        IMPORTING io_salv       TYPE REF TO cl_salv_table
                  iv_color_col  TYPE lvc_fname DEFAULT 'COLOR',

      set_layout
        IMPORTING io_salv TYPE REF TO cl_salv_table
                  iv_name TYPE string.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_alv_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_handler IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS INITIAL.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD display_storage_bins.
    DATA: lr_data TYPE REF TO data.

    " Create a copy of the data
    DATA(lt_display) = it_data.
    GET REFERENCE OF lt_display INTO lr_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        " Set columns
        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Hide internal columns
        TRY.
            lo_columns->get_column( 'COLOR' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS' )->set_visible( abap_false ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " Set column texts
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_short_text( 'StTyp' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGPLA' ) )->set_short_text( 'Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY' ) )->set_short_text( 'Occ%' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_short_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QUANT_COUNT' ) )->set_short_text( 'Quants' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAT_COUNT' ) )->set_short_text( '#Mat' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " Set color
        lo_columns->set_color_column( 'COLOR' ).

        " Enable sorting and filtering
        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        " Set zebra
        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Storage Bin Overview' ).

        " Display
        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_transfer_orders.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        TRY.
            lo_columns->get_column( 'COLOR' )->set_visible( abap_false ).
            lo_columns->get_column( 'CONFIRMED' )->set_visible( abap_false ).
          CATCH cx_salv_not_found.
        ENDTRY.

        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'TANUM' ) )->set_short_text( 'TO#' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TAPOS' ) )->set_short_text( 'Item' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_short_text( 'MvT' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_short_text( 'St' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WAIT_HOURS' ) )->set_short_text( 'Wait(h)' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        lo_columns->set_color_column( 'COLOR' ).

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Transfer Orders' ).

        " Set events for double click
        DATA(lo_events) = mo_salv->get_event( ).
        SET HANDLER on_double_click FOR lo_events.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_storage_type_summary.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        TRY.
            lo_columns->get_column( 'STATUS' )->set_visible( abap_false ).
          CATCH cx_salv_not_found.
        ENDTRY.

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Storage Type Summary' ).

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_movement_kpis.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        TRY.
            lo_columns->get_column( 'STATUS' )->set_visible( abap_false ).
          CATCH cx_salv_not_found.
        ENDTRY.

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Movement Type KPIs' ).

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_daily_statistics.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Daily Statistics' ).

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_aging_analysis.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Open TO Aging Analysis' ).

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_user_workload.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'User Workload Analysis' ).

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_material_flow.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Material Flow Analysis' ).

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_movement_simulation.
    DATA(lt_display) = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = lt_display ).

        DATA(lo_columns) = mo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        TRY.
            lo_columns->get_column( 'TIMESTAMP' )->set_visible( abap_false ).
          CATCH cx_salv_not_found.
        ENDTRY.

        DATA(lo_functions) = mo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_display) = mo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Movement Simulation Timeline' ).

        " Enable sorting
        DATA(lo_sorts) = mo_salv->get_sorts( ).
        TRY.
            lo_sorts->add_sort( columnname = 'BDATU' ).
            lo_sorts->add_sort( columnname = 'BUPTS' ).
          CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD set_column_settings.
    " Generic column settings
    io_columns->set_optimize( abap_true ).
  ENDMETHOD.

  METHOD set_color_column.
    TRY.
        DATA(lo_columns) = io_salv->get_columns( ).
        lo_columns->set_color_column( iv_color_col ).
      CATCH cx_salv_data_error.
    ENDTRY.
  ENDMETHOD.

  METHOD set_layout.
    DATA(lo_layout) = mo_salv->get_layout( ).
    DATA(ls_key) = VALUE salv_s_layout_key( report = sy-repid ).
    lo_layout->set_key( ls_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lo_layout->set_default( abap_true ).
  ENDMETHOD.

  METHOD on_user_command.
    " Handle custom toolbar functions
    CASE e_salv_function.
      WHEN 'REFRESH'.
        " Trigger refresh
        MESSAGE 'Refreshing data...' TYPE 'S'.
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    " Handle double click on TO to show details
    MESSAGE |Double clicked on row { row }, column { column }| TYPE 'S'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      go_instance TYPE REF TO lcl_controller.

    DATA:
      mo_extractor     TYPE REF TO lcl_data_extractor,
      mo_html_dash     TYPE REF TO lcl_html_dashboard,
      mo_alv_handler   TYPE REF TO lcl_alv_handler.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_controller.

    METHODS:
      initialize
        IMPORTING
          it_lgnum     TYPE lcl_data_extractor=>gty_r_lgnum
          it_lgtyp     TYPE lcl_data_extractor=>gty_r_lgtyp
          it_lgpla     TYPE lcl_data_extractor=>gty_r_lgpla
          it_matnr     TYPE lcl_data_extractor=>gty_r_matnr
          it_bwlvs     TYPE lcl_data_extractor=>gty_r_bwlvs
          iv_date_from TYPE sydatum
          iv_date_to   TYPE sydatum,

      load_all_data,

      display_overview,

      display_by_tab
        IMPORTING iv_tab TYPE i,

      calculate_global_kpis,

      get_dashboard_html
        RETURNING VALUE(rv_html) TYPE string,

      refresh_data.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS INITIAL.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD initialize.
    " Create extractor
    mo_extractor = NEW lcl_data_extractor(
      it_lgnum     = it_lgnum
      it_lgtyp     = it_lgtyp
      it_lgpla     = it_lgpla
      it_matnr     = it_matnr
      it_bwlvs     = it_bwlvs
      iv_date_from = iv_date_from
      iv_date_to   = iv_date_to ).

    " Create HTML dashboard generator
    mo_html_dash = NEW lcl_html_dashboard( ).

    " Get ALV handler
    mo_alv_handler = lcl_alv_handler=>get_instance( ).
  ENDMETHOD.

  METHOD load_all_data.
    " Load all data from extractor
    gt_storage_bins     = mo_extractor->extract_storage_bins( ).
    gt_transfer_orders  = mo_extractor->extract_transfer_orders( ).
    gt_storage_type_sum = mo_extractor->get_storage_type_summary( ).
    gt_movement_kpis    = mo_extractor->get_movement_kpis( ).
    gt_workloads        = mo_extractor->get_workload_analysis( ).
    gt_user_workloads   = mo_extractor->get_user_workload( ).
    gt_daily_stats      = mo_extractor->get_daily_statistics( ).
    gt_agings           = mo_extractor->get_aging_analysis( ).
    gt_material_flows   = mo_extractor->get_material_flow( ).
    gt_movement_sims    = mo_extractor->get_movement_simulation( ).

    " Calculate global KPIs
    calculate_global_kpis( ).

    gv_data_loaded = abap_true.
  ENDMETHOD.

  METHOD calculate_global_kpis.
    " Calculate bin metrics
    gv_total_bins = lines( gt_storage_bins ).
    gv_occupied_bins = REDUCE #(
      INIT count = 0
      FOR wa IN gt_storage_bins
      WHERE ( quant_count > 0 )
      NEXT count = count + 1 ).
    gv_empty_bins = REDUCE #(
      INIT count = 0
      FOR wa IN gt_storage_bins
      WHERE ( quant_count = 0 AND blocked <> abap_true )
      NEXT count = count + 1 ).
    gv_blocked_bins = REDUCE #(
      INIT count = 0
      FOR wa IN gt_storage_bins
      WHERE ( blocked = abap_true )
      NEXT count = count + 1 ).

    " Calculate TO metrics
    gv_total_to = lines( gt_transfer_orders ).
    gv_open_to = REDUCE #(
      INIT count = 0
      FOR wa IN gt_transfer_orders
      WHERE ( confirmed <> abap_true )
      NEXT count = count + 1 ).
    gv_confirmed_to = gv_total_to - gv_open_to.

    " Calculate average confirmation time (only for confirmed)
    DATA: lv_total_hours TYPE p LENGTH 15 DECIMALS 2,
          lv_count       TYPE i.

    LOOP AT gt_transfer_orders INTO DATA(ls_to) WHERE confirmed = abap_true.
      lv_total_hours = lv_total_hours + ls_to-wait_hours.
      lv_count = lv_count + 1.
    ENDLOOP.

    IF lv_count > 0.
      gv_avg_confirm_time = lv_total_hours / lv_count.
    ENDIF.

    " Calculate overall occupancy
    IF gv_total_bins > 0.
      gv_overall_occupancy = ( gv_occupied_bins / gv_total_bins ) * 100.
    ENDIF.

    " Calculate total quants
    gv_total_quants = REDUCE #(
      INIT total = 0
      FOR wa IN gt_storage_bins
      NEXT total = total + wa-quant_count ).
  ENDMETHOD.

  METHOD display_overview.
    " Generate and display HTML overview
    DATA(lv_html) = get_dashboard_html( ).

    " Write overview to list
    WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
    WRITE: / '                    SAP WM VISUAL MONITORING DASHBOARD'.
    WRITE: / '════════════════════════════════════════════════════════════════════════════════'.
    SKIP.

    " Storage Bin Summary
    FORMAT COLOR COL_HEADING.
    WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
    WRITE: / '│                           STORAGE BIN OVERVIEW                              │'.
    WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
    FORMAT COLOR OFF.

    WRITE: / icon_wf_workitem AS ICON, 'Total Bins:'.
    WRITE: AT 30 gv_total_bins.

    FORMAT COLOR COL_POSITIVE.
    WRITE: / icon_okay AS ICON, 'Occupied Bins:'.
    WRITE: AT 30 gv_occupied_bins.
    FORMAT COLOR OFF.

    WRITE: / icon_space AS ICON, 'Empty Bins:'.
    WRITE: AT 30 gv_empty_bins.

    FORMAT COLOR COL_NEGATIVE.
    WRITE: / icon_locked AS ICON, 'Blocked Bins:'.
    WRITE: AT 30 gv_blocked_bins.
    FORMAT COLOR OFF.

    WRITE: / icon_trend_up AS ICON, 'Occupancy Rate:'.
    WRITE: AT 30 gv_overall_occupancy, '%'.

    SKIP.

    " Transfer Order Summary
    FORMAT COLOR COL_HEADING.
    WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
    WRITE: / '│                         TRANSFER ORDER OVERVIEW                             │'.
    WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
    FORMAT COLOR OFF.

    WRITE: / icon_list AS ICON, 'Total Transfer Orders:'.
    WRITE: AT 35 gv_total_to.

    FORMAT COLOR COL_TOTAL.
    WRITE: / icon_time AS ICON, 'Open TOs (pending):'.
    WRITE: AT 35 gv_open_to.
    FORMAT COLOR OFF.

    FORMAT COLOR COL_POSITIVE.
    WRITE: / icon_okay AS ICON, 'Confirmed TOs:'.
    WRITE: AT 35 gv_confirmed_to.
    FORMAT COLOR OFF.

    WRITE: / icon_date AS ICON, 'Avg Confirmation Time:'.
    WRITE: AT 35 gv_avg_confirm_time DECIMALS 2, 'hours'.

    SKIP.

    " Status indicators
    FORMAT COLOR COL_HEADING.
    WRITE: / '┌─────────────────────────────────────────────────────────────────────────────┐'.
    WRITE: / '│                            STATUS INDICATORS                                │'.
    WRITE: / '└─────────────────────────────────────────────────────────────────────────────┘'.
    FORMAT COLOR OFF.

    " Workload status
    IF gv_open_to > ( gv_total_to / 2 ).
      FORMAT COLOR COL_NEGATIVE.
      WRITE: / icon_led_red AS ICON, 'Workload Status: HIGH BACKLOG - Action Required!'.
    ELSEIF gv_open_to > ( gv_total_to / 4 ).
      FORMAT COLOR COL_TOTAL.
      WRITE: / icon_led_yellow AS ICON, 'Workload Status: MODERATE - Monitor closely'.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: / icon_led_green AS ICON, 'Workload Status: NORMAL - All systems go'.
    ENDIF.
    FORMAT COLOR OFF.

    " Processing speed
    IF gv_avg_confirm_time > 8.
      FORMAT COLOR COL_NEGATIVE.
      WRITE: / icon_led_red AS ICON, 'Processing Speed: SLOW - Review bottlenecks'.
    ELSEIF gv_avg_confirm_time > 4.
      FORMAT COLOR COL_TOTAL.
      WRITE: / icon_led_yellow AS ICON, 'Processing Speed: AVERAGE'.
    ELSE.
      FORMAT COLOR COL_POSITIVE.
      WRITE: / icon_led_green AS ICON, 'Processing Speed: GOOD'.
    ENDIF.
    FORMAT COLOR OFF.

    SKIP.
    WRITE: / '════════════════════════════════════════════════════════════════════════════════'.

  ENDMETHOD.

  METHOD display_by_tab.
    CASE iv_tab.
      WHEN gc_tab_overview.
        display_overview( ).

      WHEN gc_tab_to.
        mo_alv_handler->display_transfer_orders( gt_transfer_orders ).

      WHEN gc_tab_bins.
        mo_alv_handler->display_storage_bins( gt_storage_bins ).

      WHEN gc_tab_kpi.
        " Show KPIs menu
        WRITE: / 'Select KPI view:'.
        WRITE: / '1. Movement Type KPIs'.
        WRITE: / '2. Daily Statistics'.
        WRITE: / '3. Aging Analysis'.
        WRITE: / '4. Material Flow'.
        mo_alv_handler->display_movement_kpis( gt_movement_kpis ).

      WHEN gc_tab_simulation.
        mo_alv_handler->display_movement_simulation( gt_movement_sims ).

      WHEN gc_tab_workload.
        mo_alv_handler->display_user_workload( gt_user_workloads ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_dashboard_html.
    rv_html = mo_html_dash->generate_overview_html(
      iv_total_bins       = gv_total_bins
      iv_occupied_bins    = gv_occupied_bins
      iv_empty_bins       = gv_empty_bins
      iv_blocked_bins     = gv_blocked_bins
      iv_total_to         = gv_total_to
      iv_open_to          = gv_open_to
      iv_confirmed_to     = gv_confirmed_to
      iv_avg_confirm_time = gv_avg_confirm_time
      iv_occupancy_pct    = gv_overall_occupancy ).
  ENDMETHOD.

  METHOD refresh_data.
    " Reload all data
    load_all_data( ).
  ENDMETHOD.

ENDCLASS.
