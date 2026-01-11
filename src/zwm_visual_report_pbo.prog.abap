*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_PBO
*&---------------------------------------------------------------------*
*& PBO (Process Before Output) Modules
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module INIT_SCREEN OUTPUT
*&---------------------------------------------------------------------*
MODULE init_screen OUTPUT.
  " Initialize screen elements if needed
  IF gv_initialized IS INITIAL.
    gv_initialized = abap_true.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module INIT_HTML_VIEWER OUTPUT
*&---------------------------------------------------------------------*
MODULE init_html_viewer OUTPUT.
  DATA: lv_html TYPE string.

  " Create HTML container if not exists
  IF go_html_viewer IS INITIAL.
    " Check if container exists
    IF go_dock_container IS INITIAL.
      CREATE OBJECT go_dock_container
        EXPORTING
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = cl_gui_docking_container=>dock_at_top
          extension = 300.
    ENDIF.

    " Create HTML viewer
    CREATE OBJECT go_html_viewer
      EXPORTING
        parent = go_dock_container.
  ENDIF.

  " Load HTML content
  IF gv_data_loaded = abap_true.
    DATA(lo_controller) = lcl_controller=>get_instance( ).
    lv_html = lo_controller->get_dashboard_html( ).

    " Convert to internal table for HTML viewer
    DATA: lt_html TYPE STANDARD TABLE OF char255,
          lv_len  TYPE i,
          lv_pos  TYPE i.

    lv_len = strlen( lv_html ).
    lv_pos = 0.

    WHILE lv_pos < lv_len.
      DATA(lv_chunk) = substring( val = lv_html off = lv_pos len = nmin( val1 = 255 val2 = lv_len - lv_pos ) ).
      APPEND lv_chunk TO lt_html.
      lv_pos = lv_pos + 255.
    ENDWHILE.

    " Load HTML
    DATA: lv_url TYPE char255.
    go_html_viewer->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html ).

    go_html_viewer->show_url( url = lv_url ).
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV OUTPUT
*&---------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  " Create ALV container if not exists
  IF go_alv_container IS INITIAL.
    CREATE OBJECT go_alv_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.
  ENDIF.

  " Display ALV based on active tab
  IF gv_data_loaded = abap_true.
    DATA(lo_controller) = lcl_controller=>get_instance( ).
    lo_controller->display_by_tab( gv_active_tab ).
  ENDIF.
ENDMODULE.
