*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_PBO
*&---------------------------------------------------------------------*
*& PBO Modules for Graphical Version - Splitter Layout
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_GRAPH'.
  SET TITLEBAR 'TITLE_GRAPH'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module INIT_SCREEN OUTPUT
*&---------------------------------------------------------------------*
MODULE init_screen OUTPUT.
  DATA: lt_html      TYPE STANDARD TABLE OF w3html,
        ls_html      TYPE w3html,
        lv_url       TYPE c LENGTH 255,
        lv_html_str  TYPE string,
        lv_offset    TYPE i,
        lv_len       TYPE i,
        lv_remaining TYPE i.

  DATA: lo_controller TYPE REF TO lcl_controller_graph.

  IF gv_graph_initialized = abap_false.

    " Create custom container for splitter
    IF go_custom_container IS INITIAL.
      CREATE OBJECT go_custom_container
        EXPORTING
          container_name = 'CC_MAIN'
        EXCEPTIONS
          OTHERS         = 1.
    ENDIF.

    " Create splitter with top (dashboard) and bottom (ALV)
    IF go_splitter_main IS INITIAL AND go_custom_container IS BOUND.
      CREATE OBJECT go_splitter_main
        EXPORTING
          parent  = go_custom_container
          rows    = 2
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      " Set row heights (35% dashboard, 65% ALV)
      go_splitter_main->set_row_height(
        EXPORTING
          id     = 1
          height = gc_split_top ).

      " Get containers
      go_cont_dashboard = go_splitter_main->get_container(
        row    = 1
        column = 1 ).

      go_cont_alv = go_splitter_main->get_container(
        row    = 2
        column = 1 ).
    ENDIF.

    " Create HTML viewer for dashboard
    IF go_html_dashboard IS INITIAL AND go_cont_dashboard IS BOUND.
      CREATE OBJECT go_html_dashboard
        EXPORTING
          parent = go_cont_dashboard
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.

    " Set ALV container
    lo_controller = lcl_controller_graph=>get_instance( ).
    lo_controller->mo_alv_handler->set_container( go_cont_alv ).

    gv_graph_initialized = abap_true.
  ENDIF.

  " Load dashboard or simulation HTML based on current view
  IF go_html_dashboard IS BOUND.
    lo_controller = lcl_controller_graph=>get_instance( ).

    " Use simulation HTML for view 7, otherwise use dashboard
    IF lo_controller->get_current_view( ) = 7.
      lv_html_str = lo_controller->get_simulation_html( ).
    ELSE.
      lv_html_str = lo_controller->get_dashboard_html( ).
    ENDIF.

    " Convert HTML string to table (w3html has 255 chars)
    CLEAR lt_html.
    lv_offset = 0.
    lv_remaining = strlen( lv_html_str ).

    WHILE lv_remaining > 0.
      IF lv_remaining > 255.
        lv_len = 255.
      ELSE.
        lv_len = lv_remaining.
      ENDIF.

      ls_html = lv_html_str+lv_offset(lv_len).
      APPEND ls_html TO lt_html.
      lv_offset = lv_offset + lv_len.
      lv_remaining = lv_remaining - lv_len.
    ENDWHILE.

    " Load HTML into viewer
    go_html_dashboard->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html ).

    go_html_dashboard->show_url( url = lv_url ).
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV OUTPUT
*&---------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  DATA: lo_ctrl TYPE REF TO lcl_controller_graph.

  lo_ctrl = lcl_controller_graph=>get_instance( ).

  " Only recreate ALV if view changed or first time
  IF gv_last_view_pbo <> lo_ctrl->get_current_view( ) OR
     lo_ctrl->mo_alv_handler->mo_salv IS NOT BOUND.

    " Free previous SALV instance
    IF lo_ctrl->mo_alv_handler->mo_salv IS BOUND.
      FREE lo_ctrl->mo_alv_handler->mo_salv.
    ENDIF.

    lo_ctrl->display_current_view( ).
    gv_last_view_pbo = lo_ctrl->get_current_view( ).
  ENDIF.
ENDMODULE.
