*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_PAI
*&---------------------------------------------------------------------*
*& PAI (Process After Input) Modules
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA(lv_okcode) = gv_okcode.
  CLEAR gv_okcode.

  CASE lv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      " Clean up
      PERFORM cleanup_gui_objects.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
      " Refresh data
      DATA(lo_controller) = lcl_controller=>get_instance( ).
      lo_controller->refresh_data( ).
      MESSAGE 'Data refreshed successfully' TYPE 'S'.

    WHEN 'TAB1'.  " Overview
      gv_active_tab = gc_tab_overview.

    WHEN 'TAB2'.  " Transfer Orders
      gv_active_tab = gc_tab_to.

    WHEN 'TAB3'.  " Storage Bins
      gv_active_tab = gc_tab_bins.

    WHEN 'TAB4'.  " KPIs
      gv_active_tab = gc_tab_kpi.

    WHEN 'TAB5'.  " Simulation
      gv_active_tab = gc_tab_simulation.

    WHEN 'TAB6'.  " Workload
      gv_active_tab = gc_tab_workload.

    WHEN 'EXPORT'.
      PERFORM export_to_excel.

    WHEN 'PRINT'.
      PERFORM print_report.

    WHEN OTHERS.
      " Handle other commands
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module EXIT_COMMAND INPUT
*&---------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE gv_okcode.
    WHEN 'EXIT' OR 'CANC'.
      PERFORM cleanup_gui_objects.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
