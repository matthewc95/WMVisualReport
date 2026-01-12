*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_PAI
*&---------------------------------------------------------------------*
*& PAI Modules for Graphical Version
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lo_ctrl_pai TYPE REF TO lcl_controller_graph.
  DATA: lv_ucomm TYPE sy-ucomm.

  " Get function code from sy-ucomm (standard SAP variable)
  lv_ucomm = sy-ucomm.
  CLEAR sy-ucomm.

  lo_ctrl_pai = lcl_controller_graph=>get_instance( ).

  CASE lv_ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      " Free objects
      IF go_html_dashboard IS BOUND.
        go_html_dashboard->free( ).
        FREE go_html_dashboard.
      ENDIF.
      IF go_splitter_main IS BOUND.
        go_splitter_main->free( ).
        FREE go_splitter_main.
      ENDIF.
      IF go_custom_container IS BOUND.
        go_custom_container->free( ).
        FREE go_custom_container.
      ENDIF.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
      " Refresh data
      lo_ctrl_pai->refresh_data( ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_BINS'.
      lo_ctrl_pai->set_view( 1 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_TO'.
      lo_ctrl_pai->set_view( 2 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_KPI'.
      lo_ctrl_pai->set_view( 3 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_STSUM'.
      lo_ctrl_pai->set_view( 4 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_DAILY'.
      lo_ctrl_pai->set_view( 5 ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_USERS'.
      lo_ctrl_pai->set_view( 6 ).
      gv_graph_initialized = abap_false.

  ENDCASE.
ENDMODULE.
