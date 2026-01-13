*&---------------------------------------------------------------------*
*& Report ZWM_VISUAL_REPORT_GRAPH
*&---------------------------------------------------------------------*
*& SAP WM Visual Monitoring Dashboard - Enhanced Graphical Version
*& Dashboard SAP WM per Monitoraggio Visuale - Versione Grafica Avanzata
*&
*& ENGLISH:
*& This report provides a modern graphical interface for SAP Warehouse
*& Management monitoring. It combines an HTML-based dashboard with
*& traditional ALV grid display using a splitter layout.
*&
*& Key Features:
*& - Modern HTML dashboard with KPI cards, progress rings, and charts
*& - Splitter layout: dashboard (35%) above, ALV grid (65%) below
*& - Row coloring in ALV tables using LVC_T_SCOL
*& - Multiple views: Bins, TOs, KPIs, Storage Types, Daily Stats, Users
*& - Movement simulation with animated warehouse visualization
*& - Hotspots for drill-down navigation
*&
*& ITALIANO:
*& Questo report fornisce un'interfaccia grafica moderna per il
*& monitoraggio della gestione magazzino SAP. Combina una dashboard
*& HTML con la visualizzazione ALV tradizionale usando uno splitter.
*&
*& Funzionalità Principali:
*& - Dashboard HTML moderna con KPI cards, anelli di progresso, grafici
*& - Layout splitter: dashboard (35%) sopra, griglia ALV (65%) sotto
*& - Colorazione righe nelle tabelle ALV tramite LVC_T_SCOL
*& - Viste multiple: Bin, TO, KPI, Storage Type, Statistiche, Utenti
*& - Simulazione movimenti con visualizzazione animata del magazzino
*& - Hotspot per navigazione drill-down
*&
*& Technical Architecture / Architettura Tecnica:
*& - Reuses base classes from ZWM_VISUAL_REPORT (lcl_data_extractor)
*& - Riutilizza classi base da ZWM_VISUAL_REPORT (lcl_data_extractor)
*& - Extends with graphical capabilities (lcl_html_dashboard_modern)
*& - Estende con capacità grafiche (lcl_html_dashboard_modern)
*&
*& Author: Generated with Claude AI assistance
*& Date: 2024
*&---------------------------------------------------------------------*
REPORT zwm_visual_report_graph.

*----------------------------------------------------------------------*
* Includes / File Inclusi
*----------------------------------------------------------------------*
* Base includes from original report (reused for data extraction)
* Include di base dal report originale (riutilizzati per estrazione dati)
INCLUDE zwm_visual_report_top.            " Base types and globals / Tipi e variabili base
INCLUDE zwm_visual_report_cls.            " Base classes / Classi base

* Extended includes for graphical version
* Include estesi per versione grafica
INCLUDE zwm_visual_report_graph_top.      " Extended types with LVC_T_SCOL / Tipi estesi con LVC_T_SCOL
INCLUDE zwm_visual_report_graph_cls.      " Enhanced graphical classes / Classi grafiche avanzate
INCLUDE zwm_visual_report_graph_scr.      " Selection screen / Schermata selezione
INCLUDE zwm_visual_report_graph_pbo.      " PBO modules / Moduli PBO
INCLUDE zwm_visual_report_graph_pai.      " PAI modules / Moduli PAI
INCLUDE zwm_visual_report_graph_frm.      " Form routines / Routine FORM

*----------------------------------------------------------------------*
* Initialization / Inizializzazione
*----------------------------------------------------------------------*
* Set default values for selection screen parameters
* Imposta valori predefiniti per i parametri della schermata di selezione
INITIALIZATION.
  " Default date range: last 30 days
  " Intervallo date predefinito: ultimi 30 giorni
  p_datto = sy-datum.
  p_datfr = sy-datum - 30.

*----------------------------------------------------------------------*
* At Selection Screen / Alla Schermata di Selezione
*----------------------------------------------------------------------*
* Input validation before execution
* Validazione input prima dell'esecuzione
AT SELECTION-SCREEN.
  " Validate date range: From date must be <= To date
  " Valida intervallo date: Data Da deve essere <= Data A
  IF p_datfr > p_datto.
    MESSAGE e001(00) WITH 'Date From cannot be greater than Date To'.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection / Inizio Selezione
*----------------------------------------------------------------------*
* Main execution flow
* Flusso di esecuzione principale
START-OF-SELECTION.
  " Step 1: Initialize controller singleton with selection parameters
  " Passo 1: Inizializza singleton controller con parametri di selezione
  PERFORM initialize_controller.

  " Step 2: Load all warehouse data (bins, TOs, KPIs, etc.)
  " Passo 2: Carica tutti i dati magazzino (bin, TO, KPI, ecc.)
  PERFORM load_data.

  " Step 3: Display main screen with splitter layout
  " Passo 3: Visualizza schermata principale con layout splitter
  CALL SCREEN gc_dynnr_graph.
