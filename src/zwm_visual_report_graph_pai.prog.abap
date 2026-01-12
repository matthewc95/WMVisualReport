*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_PAI
*&---------------------------------------------------------------------*
*& PAI Modules for Graphical Version
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA(lo_controller) = lcl_controller_graph=>get_instance( ).

  CASE gv_okcode_graph.
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
      lo_controller->refresh_data( ).
      gv_graph_initialized = abap_false.

    WHEN 'VIEW_BINS'.
      lo_controller->set_view( 1 ).

    WHEN 'VIEW_TO'.
      lo_controller->set_view( 2 ).

    WHEN 'VIEW_KPI'.
      lo_controller->set_view( 3 ).

    WHEN 'VIEW_STSUM'.
      lo_controller->set_view( 4 ).

    WHEN 'VIEW_DAILY'.
      lo_controller->set_view( 5 ).

    WHEN 'VIEW_USERS'.
      lo_controller->set_view( 6 ).

  ENDCASE.

  CLEAR gv_okcode_graph.
ENDMODULE.
