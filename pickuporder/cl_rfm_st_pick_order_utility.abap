class CL_RFM_ST_PICK_ORDER_UTILITY definition
  public
  final
  create private

  global friends CL_RFM_ST_PICK_ORDER_FACTORY .

public section.

  interfaces IF_RFM_ST_PICK_ORDER_UTILITY .

  constants GV_PICKINGSTATUS_HANDEDOVER type RFM_ST_ORDER_PICKING_STATUS value '05' ##NO_TEXT.
  constants GV_OVERALLSTATUS_HANDEDOVER type RFM_ST_ORDER_OVERALL_STATUS value 'H' ##NO_TEXT.
  constants GV_PICKINGSTATUS_RESVHANDOVR type RFM_ST_ORDER_PICKING_STATUS value '06' ##NO_TEXT.
  constants GV_PICKINGSTATUS_CANCELORDER type RFM_ST_ORDER_PICKING_STATUS value '07' ##NO_TEXT.
  constants MV_ORDR_PAID type RFM_ST_ORDER_PAYMENT_STATUS value '1' ##NO_TEXT.
  constants MV_ORDR_UNPAID type RFM_ST_ORDER_PAYMENT_STATUS value '2' ##NO_TEXT.
  constants GV_OVERALLSTATUS_REJECTED type RFM_ST_ORDER_OVERALL_STATUS value 'R' ##NO_TEXT.
  constants GV_OVERALLSTATUS_RES_HANDOVER type RFM_ST_ORDER_OVERALL_STATUS value 'T' ##NO_TEXT.
  constants MV_CC_ORDER type STRING value 'CC' ##NO_TEXT.
  constants MV_CR_ORDER type STRING value 'CR' ##NO_TEXT.
protected section.

  data GO_APPLICATION_LOG type ref to CL_RTST_APPLICATION_LOG .
private section.

  data GT_HANDOVER_DATA type TDT_LIPS .

  class-methods CREATE
    returning
      value(RO_ST_UTILITY) type ref to IF_RFM_ST_PICK_ORDER_UTILITY .
  methods CONSTRUCTOR .
ENDCLASS.



CLASS CL_RFM_ST_PICK_ORDER_UTILITY IMPLEMENTATION.


METHOD if_rfm_st_pick_order_utility~get_orders_from_requests.

  IF it_pick_requests IS NOT INITIAL.

    SELECT DISTINCT * FROM rfm_st_pick_ord INTO TABLE et_pick_orders
                                           FOR ALL ENTRIES IN it_pick_requests
                                           WHERE store = it_pick_requests-store
                                           AND storepickuporder = it_pick_requests-referencestorepickuporder.

  ENDIF.

ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_payment_status.

    DATA lv_aunum TYPE aunum.
    IF iv_vbeln IS NOT INITIAL.
      SELECT SINGLE aunum FROM fpltc
         JOIN vbak ON fpltc~fplnr = vbak~rplnr
         WHERE vbak~vbeln = @iv_vbeln
         AND fpltc~aunum IS NOT INITIAL
         INTO @lv_aunum.

      IF lv_aunum IS NOT INITIAL.
        ev_status = mv_ordr_paid.
      ELSE.
        ev_status = mv_ordr_unpaid.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_rejection_reason_code.

    SELECT SINGLE reason_for_short_picking  FROM rfm_c_pick_ord INTO  ev_rejection_reason_code.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_t001w_single.
    IF iv_werks IS NOT INITIAL.
      CALL FUNCTION 'T001W_SINGLE_READ'
        EXPORTING
          t001w_werks = iv_werks
        IMPORTING
          wt001w      = es_t001w
        EXCEPTIONS
          not_found   = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_vbak_array.
    DATA: lt_vbeln TYPE tt_vbeln,
          lt_vbak  TYPE vbak_t.
    IF it_vbeln IS NOT INITIAL.
      CALL FUNCTION 'SD_VBAK_ARRAY_READ'
        TABLES
          it_vbak_key           = lt_vbeln
          et_vbak               = lt_vbak
        EXCEPTIONS
          records_not_found     = 1
          records_not_requested = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
      ELSE.
        et_vbak = lt_vbak.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_vbak_single.
      DATA: ls_return TYPE bapiret2.
    IF iv_vbeln IS NOT INITIAL.
      CALL FUNCTION 'SD_VBAK_SINGLE_READ'
        EXPORTING
          i_vbeln          = iv_vbeln
        IMPORTING
          e_vbak           = es_vbak
        EXCEPTIONS
          record_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        IF sy-subrc = 1.
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-number = '001'.
          ls_return-type = 'E'.
          ls_return-message_v1 = iv_vbeln.
          APPEND ls_return TO et_return.
          IF 1 = 2. MESSAGE e001(RFM_ST_PICK_ORD) WITH iv_vbeln. ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_vbap.
    DATA: ls_return TYPE bapiret2.
    IF iv_vbeln IS NOT INITIAL.
      CALL FUNCTION 'SD_VBAP_READ_WITH_VBELN'
        EXPORTING
          i_vbeln          = iv_vbeln
        TABLES
          et_vbap          = et_vbap
        EXCEPTIONS
          record_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        IF sy-subrc = 1.
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-number = '001'.
          ls_return-message_v1 = iv_vbeln.
          APPEND ls_return TO et_return.
          IF 1 = 2. MESSAGE e001(rfm_st_pick_ord) WITH iv_vbeln. ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_vbep_with_posnr.
    DATA ls_return TYPE bapiret2.
    IF i_vbeln IS NOT INITIAL AND i_posnr IS NOT INITIAL .

      CALL FUNCTION 'SD_VBEP_READ_WITH_POSNR'
        EXPORTING
          i_vbeln          = i_vbeln
          i_posnr          = i_posnr
        TABLES
          et_vbep          = et_vbep
        EXCEPTIONS
          record_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            ls_return-id = 'RFM_ST_PICK_ORD'.
            ls_return-type = 'E'.
            ls_return-number = '003'.
            ls_return-message_v1 = i_vbeln.
            ls_return-message_v2 = i_posnr.
            append ls_return to et_return.
            IF 1 = 2 .
              MESSAGE e003(rfm_st_pick_ord) WITH i_vbeln i_posnr.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
* Implement suitable error handling here
      ENDIF.


    ENDIF.
    IF i_vbeln IS NOT INITIAL AND i_posnr IS INITIAL.
      CALL FUNCTION 'SD_VBEP_READ_WITH_VBELN'
        EXPORTING
          i_vbeln          = i_vbeln
*         I_BYPASSING_BUFFER       = ' '
          i_refresh_buffer = 'X'
        TABLES
*         ET_VBEPVB        =
          et_vbep          = et_vbep
        EXCEPTIONS
          record_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            ls_return-id = 'RFM_ST_PICK_ORD'.
            ls_return-type = 'E'.
            ls_return-number = '004'.
            ls_return-message_v1 = i_vbeln.
            append ls_return to et_return.
            IF 1 = 2 .
              MESSAGE e004(rfm_st_pick_ord) WITH i_vbeln .
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~insert_st_pick_orders.

    DATA: ls_return      TYPE bapiret2,
          ls_pp_orders_e TYPE rfm_st_pick_ord,
          lt_pp_orders   TYPE rfm_st_pick_ord_t.

    IF it_pp_orders IS NOT INITIAL.

      LOOP AT it_pp_orders INTO DATA(ls_pp_orders).
*       Get organizational data
        CALL METHOD me->if_rfm_st_pick_order_utility~get_vbak_single
          EXPORTING
            iv_vbeln = ls_pp_orders-storepickuporder
          IMPORTING
            es_vbak  = DATA(ls_salesorderheader)
*           et_return =
          .

** Authorization Check at sales are and sales document type level

        IF me->if_rfm_st_pick_order_utility~check_authorization( i_activity = '01' i_auart = ls_salesorderheader-auart i_spart = ls_Salesorderheader-spart
                                                i_vkorg = ls_Salesorderheader-vkorg i_vtweg = ls_Salesorderheader-vtweg ) = abap_false.
          CONCATENATE ls_salesorderheader-vkorg '/' ls_salesorderheader-vtweg ls_salesorderheader-spart INTO DATA(lv_sales_area).
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'E'.
          ls_return-number = '015'.
          ls_return-message_v1 = ls_pp_orders-storepickuporder.
          ls_return-message_v2 = lv_sales_area.
          ls_return-message_v3 = ls_salesorderheader-auart.
          APPEND ls_return TO et_return.
          IF 1 = 2. MESSAGE e015(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
          CONTINUE.
        ENDIF.
        ls_pp_orders-createdbyuser = sy-uname.
        GET TIME STAMP FIELD ls_pp_orders-creationdatetime.
        APPEND ls_pp_orders TO lt_pp_orders.

      ENDLOOP.

*      MODIFY rfm_st_pick_ord FROM TABLE lt_pp_orders.
      INSERT rfm_st_pick_ord FROM TABLE lt_pp_orders.

      IF  sy-subrc IS INITIAL.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'S'.
        ls_return-number = '012'.
        ls_return-message_v1 = ls_pp_orders-storepickuporder.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e012(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
*        ls_return-number = '012'.
        ls_return-message_v1 = ls_pp_orders-storepickuporder.
        APPEND ls_return TO et_return.
*        IF 1 = 2. MESSAGE e012(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~open_sales_order.
*    DATA: lrt_error TYPE mesg.
*    DATA: lv_text_exc TYPE string.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    TRY.
        CALL METHOD cl_goal_api=>so_instance->open
          EXPORTING
            iv_bo_id     = 'SALESORDER'
            iv_bo_key    = iv_bo_key
            iv_read_only = abap_false
          RECEIVING
            ro_access    = er_access.
      CATCH cx_goal_exc INTO lx_goal_exc.
*        lv_text_exc = lx_goal_exc->get_text( ).
*        lrt_error- = 'RFM_ST_PICK_REQ'.
*        lrt_error-msgno = '015'.
*        lrt_error-msgty = 'E'.
*        lrt_error-msgv1 = iv_vbeln.
*        lrt_error-msgv2 = lv_text_exc.
*        APPEND lrt_error TO rt_message.
*        IF 1 = 2. MESSAGE e015(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~pod_delivery_update.
     "Post Proof Of Delivery
    CLEAR ev_subrc.
    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      EXPORTING
        vbkok_wa                = is_vbkok
        synchron                = iv_synchron
        delivery                = iv_delivery_id
        update_picking          = iv_update_picking
        commit                  = iv_commit
      IMPORTING
        ef_error_any            = ev_error_any
        ef_error_in_goods_issue = ev_error_in_goods_issue
      TABLES
        vbpok_tab               = ct_vbpok
        prot                    = ct_prot
      EXCEPTIONS
        error_message           = 1.
    ev_subrc = sy-subrc.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~post_proof_of_delivery.
    DATA ls_vbkok                 TYPE vbkok.
    DATA lv_error_any             TYPE abap_boolean.
    DATA lv_error_in_goods_issue  TYPE abap_boolean.
    DATA lt_prot                  TYPE STANDARD TABLE OF prott.
    DATA ls_message               TYPE cl_rtst_application_log=>ts_message.
    DATA: lt_lips   TYPE TABLE OF lips.
    IF iv_delivery_id IS INITIAL.
      RETURN.
    ENDIF.
    "Fill data for Proof of delivery
    ls_vbkok-vbeln_vl = iv_delivery_id.
    ls_vbkok-podat = sy-datum.
    ls_vbkok-potim = sy-timlo.
    ls_vbkok-kzpod = 'D'. "POD indicator
    ls_vbkok-wabuc = abap_true. "Post Goods Movement
    lt_lips = gt_handover_data.
    DATA(lt_vbpok) = VALUE  tt_vbpok( FOR ls_lips IN lt_lips WHERE ( vbeln = iv_delivery_id )
    (
         vbeln_vl = ls_lips-vbeln
         posnr_vl = ls_lips-posnr
         lianp    = 'X'
         lfimg    = ls_lips-lfimg
         lgmng    = ls_lips-lgmng
       )
       ).

    if_rfm_st_pick_order_utility~pod_delivery_update(
    EXPORTING
      is_vbkok                = ls_vbkok
      iv_synchron             = abap_false
      iv_delivery_id          = iv_delivery_id
      iv_update_picking       = abap_false
      iv_commit               = abap_true
    IMPORTING
      ev_error_any            = lv_error_any
      ev_error_in_goods_issue = lv_error_in_goods_issue
      ev_subrc                = DATA(lv_subrc)
    CHANGING
      ct_vbpok                = lt_vbpok
      ct_prot                 = lt_prot
  ).
    IF lv_subrc <> 0.
*       make sure any error is set
      lv_error_any = abap_true.
*       add error message
      IF NOT line_exists(
               lt_prot[ msgid = sy-msgid msgno = sy-msgno msgty = 'E' msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ] ).
        APPEND VALUE #( msgid = sy-msgid msgno = sy-msgno msgty = 'E' msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) TO lt_prot.
      ENDIF.
    ENDIF.
*    IF lv_error_any IS NOT INITIAL.
      " Fill error message into exporting table
      LOOP AT lt_prot REFERENCE INTO DATA(lrs_prot).
        ls_message-msgid = lrs_prot->msgid.
        ls_message-msgno = lrs_prot->msgno.
        ls_message-msgty = lrs_prot->msgty.
        ls_message-msgv1 = lrs_prot->msgv1.
        ls_message-msgv2 = lrs_prot->msgv2.
        ls_message-msgv3 = lrs_prot->msgv3.
        ls_message-msgv4 = lrs_prot->msgv4.
        APPEND ls_message TO et_messages.
      ENDLOOP.
*      RETURN.
*    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~print_box_labels.

    CALL METHOD cl_rfm_st_pkord_box_labels=>if_rfm_st_pkord_box_labels~print_box_labels
      EXPORTING
        iv_formname = iv_formname
        iv_printer  = iv_printer
        it_formdata = it_formdata.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~print_content_list.

    CALL METHOD cl_rfm_st_pkord_content_list=>if_rfm_st_pkord_content_list~print_content_list
      EXPORTING
        iv_formname = iv_formname
        iv_printer  = iv_printer
        it_formdata = it_formdata.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~read_c_pick_ord.


    DATA ls_c_pick_store TYPE rfm_c_pick_ord.
    SELECT SINGLE * FROM rfm_c_pick_ord INTO ls_c_pick_store.

    IF sy-subrc IS INITIAL  .
      es_c_pp_store = ls_c_pick_store.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~read_st_pick_orders.

    DATA: ls_return TYPE bapiret2,
          lv_dummy  TYPE bapi_msg.

    " only if single order needs to be read
    IF  iv_st_order IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_ord WHERE storepickuporder = @iv_st_order INTO TABLE @et_pp_orders.
      IF sy-subrc IS NOT INITIAL.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        ls_return-number = '059'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e059(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.

    " if multiple orders need to be read
    IF it_pp_orders IS NOT INITIAL.
      SELECT DISTINCT * FROM rfm_st_pick_ord FOR ALL ENTRIES IN @it_pp_orders
                                                    WHERE storepickuporder = @it_pp_orders-storepickuporder
                                                    INTO TABLE @et_pp_orders.
      IF sy-subrc IS NOT INITIAL.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        ls_return-number = '059'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e059(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
      ELSE.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~reset_release_status.

    DATA: lt_ref_ord TYPE rfm_st_pick_ord_t.

    IF it_ord IS NOT INITIAL.

      lt_ref_ord = it_ord.

      LOOP AT lt_ref_ord  ASSIGNING FIELD-SYMBOL(<ls_ref_ord>).

        READ TABLE it_req WITH KEY store =  <ls_ref_ord>-store referencestorepickuporder = <ls_ref_ord>-storepickuporder
                                   storepickingrequeststatus = 'A' TRANSPORTING NO FIELDS.

        IF sy-subrc <> 0.

          READ TABLE it_req WITH KEY store =  <ls_ref_ord>-store referencestorepickuporder = <ls_ref_ord>-storepickuporder
                                   storepickingrequeststatus = 'B' TRANSPORTING NO FIELDS.

          IF sy-subrc <> 0.
            <ls_ref_ord>-storeorderpickingstatus = '03'.
            <ls_ref_ord>-storeorderoverallstatus = 'O'.
          ENDIF.

        ENDIF.

      ENDLOOP.

      UPDATE rfm_st_pick_ord FROM TABLE lt_ref_ord.

    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~reverse_post_goods_issue.
    DATA: lt_mesg            TYPE STANDARD TABLE OF mesg.
    "Submit report to reverse goods issue
    SUBMIT r_wsso_cancel_goods_issue WITH dlv_numb = iv_delivery AND RETURN.

    IMPORT lt_mesg = lt_mesg FROM MEMORY ID cl_wsso_dlv_cancel=>gc_mem_id_mesg.

    READ TABLE lt_mesg WITH KEY msgty = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      ROLLBACK WORK.
      rt_messages =  lt_mesg.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~set_entity_set.
    CALL METHOD iv_access->set_entity_set
      EXPORTING
        iv_entity_id     = 'ITEM'
        it_entity_data   = it_entity_data
        it_changed_field = it_changed_field.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~set_rejection_text.
    DATA lt_line TYPE TABLE OF tline.
    DATA lv_line TYPE TABLE OF tline.
    DATA ls_line TYPE tline.
    IF it_rejection_text IS INITIAL OR is_thead IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT it_rejection_text INTO DATA(lv_text).
      ls_line-tdline = lv_text.
      ls_line-tdformat = '*'.
      APPEND ls_line TO lt_line.
    ENDLOOP.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = is_thead
        savemode_direct = 'X'
      TABLES
        lines           = lt_line
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      ev_error = sy-subrc.
      RETURN.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~update_picking_do_pgi.
    IF is_vbkok IS INITIAL.
      RETURN.
    ENDIF.
    CLEAR ev_subrc.
    "Logic to check existing lock on delivery
    DATA(lv_lock) = abap_true.
    WHILE ( lv_lock EQ abap_true ).
      "Try to lock delivery first
      CALL FUNCTION 'SHP_ENQUEUE_EVVBLKE'
        EXPORTING
          mode_likp         = 'E'
          mandt             = sy-mandt
          vbeln             = iv_delivery_id
        EXCEPTIONS
          foreign_lock      = 1
          system_failure    = 2
          csl_no_connection = 3
          csl_inconsistency = 4
          OTHERS            = 5.
      IF sy-subrc EQ 0. "If we are able to lock the delivery then no lock exists on delivery before

        CALL FUNCTION 'SHP_DEQUEUE_EVVBLKE' "Unlock the locked delivery for further processing
          EXPORTING
            mode_likp = 'E'
            mandt     = sy-mandt
            vbeln     = iv_delivery_id.
        lv_lock = abap_false.
      ELSE.
        WAIT UP TO 1 SECONDS. "If locking on delivery fails then wait for 1 sec.
      ENDIF.
    ENDWHILE.

    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      EXPORTING
        vbkok_wa                = is_vbkok
        synchron                = iv_synchron
        delivery                = iv_delivery_id
        update_picking          = iv_update_picking
        commit                  = iv_commit
      IMPORTING
        ef_error_any            = ev_error_any
        ef_error_in_goods_issue = ev_error_in_goods_issue
      TABLES
        vbpok_tab               = ct_vbpok
        prot                    = ct_prot
      EXCEPTIONS
        error_message           = 1.

    ev_subrc = sy-subrc.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~update_st_pick_orders.


    DATA: ls_return    TYPE bapiret2,
          lt_pp_orders TYPE rfm_st_pick_ord_t.
    IF it_pp_orders IS NOT INITIAL .

      LOOP AT it_pp_orders INTO DATA(ls_pp_orders).

*        Get organizational data
        CALL METHOD me->if_rfm_st_pick_order_utility~get_vbak_single
          EXPORTING
            iv_vbeln = ls_pp_orders-storepickuporder
          IMPORTING
            es_vbak  = DATA(ls_salesorderheader).

** Authorization Check at sales are and sales document type level
        IF me->if_rfm_st_pick_order_utility~check_authorization( i_activity = '02' i_auart = ls_Salesorderheader-auart i_spart = ls_Salesorderheader-spart
                                                i_vkorg = ls_Salesorderheader-vkorg i_vtweg = ls_Salesorderheader-vtweg ) = abap_false.


          go_application_log->add_message(
           EXPORTING
             iv_msgno = 061
             iv_msgty = 'E'
             iv_msgid = 'RFM_ST_PICK_ORD' ).
          IF 1 = 2. MESSAGE e061(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.

          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'E'.
          ls_return-number = '017'.
          CONCATENATE ls_salesorderheader-vkorg '/' ls_salesorderheader-vtweg '/' ls_salesorderheader-spart INTO DATA(lv_sales_area).
          ls_return-message_v1 = ls_salesorderheader-vbeln.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          ls_return-message_v2 = lv_sales_area.
          ls_return-message_v3 = ls_salesorderheader-auart.
          APPEND ls_return TO et_return.
          IF 1 = 2. MESSAGE e016(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
          CONTINUE.
        ENDIF.
        GET TIME STAMP FIELD ls_pp_orders-lastchangedatetime.
        ls_pp_orders-lastchangedbyuser = sy-uname.
        APPEND ls_pp_orders TO lt_pp_orders.

      ENDLOOP.

      UPDATE rfm_st_pick_ord FROM TABLE @lt_pp_orders.

      IF sy-subrc IS INITIAL .
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'S'.
        ls_return-number = '010'.
        LOOP AT lt_pp_orders INTO DATA(ls_pp_orders_upd).
          ls_return-message_v1 = ls_pp_orders_upd-storepickuporder.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          APPEND ls_return TO et_return.
        ENDLOOP.
        IF 1 = 2. MESSAGE e010(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        ls_return-number = '011'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e011(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
      ENDIF.

    ENDIF.
    go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_picking_req_api=>gc_appl_log_subobject ).
  ENDMETHOD.


  METHOD constructor.

    go_application_log = cl_rtst_application_log=>get_instance( ).

  ENDMETHOD.


  method CREATE.

    ro_st_utility = NEW cl_rfm_st_pick_order_utility( ).
  endmethod.


  METHOD if_rfm_st_pick_order_utility~check_authorization.
    ev_allowed = abap_false.
    AUTHORITY-CHECK OBJECT 'V_VBAK_AAT'
      ID 'ACTVT' FIELD i_activity
      ID 'AUART' FIELD i_auart.

    DATA(subrc_aat) = sy-subrc.

    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
      ID 'ACTVT' FIELD i_activity
      ID 'SPART' FIELD i_spart
      ID 'VKORG' FIELD i_vkorg
      ID 'VTWEG' FIELD i_vtweg.

    DATA(subrc_vko) = sy-subrc.

    CHECK subrc_vko = 0 AND subrc_aat = 0.
    ev_allowed = abap_true.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~close_sales_order.
    IF iv_access IS NOT INITIAL.
      IF iv_save EQ abap_true.
        iv_access->save( iv_no_commit = abap_false ).
      ENDIF.

      TRY.
          iv_access->close(
               iv_delete_trace = abap_true
           ).
        CATCH cx_goal_exc. " GOAL: Exception
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~commit_work.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = es_return.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~create_billing.
    TYPES: BEGIN OF lty_lips,
             vbeln TYPE lips-vbeln,
             posnr TYPE lips-posnr,
           END OF lty_lips.
    DATA: lt_success TYPE STANDARD TABLE OF bapivbrksuccess,
          lt_lips    TYPE STANDARD TABLE OF lty_lips.
    DATA: lt_billing_data TYPE STANDARD TABLE OF bapivbrk,
          ls_return       TYPE bapiret2.

    IF it_deliveries IS NOT INITIAL.
      DATA(Range_deliveries) = VALUE rseloption( FOR ls_delv IN it_deliveries ( sign = 'I' option = 'EQ' low = ls_delv ) ).

      SELECT vbeln,posnr FROM lips INTO TABLE @lt_lips  WHERE vbeln IN @Range_deliveries.
      IF sy-subrc <> 0.
        "Log error message
        LOOP AT it_deliveries INTO DATA(ls_vbeln_vl).
          clear ls_return.
          ls_return-number = '043'.
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'E'.
          ls_return-message_v1 = ls_vbeln_vl.
          APPEND ls_return TO et_bapiret2.
          IF 1 = 2. MESSAGE E043(RFM_ST_PICK_ORD) INTO DATA(lv_dumy). ENDIF.
        ENDLOOP.
        RETURN.
      ENDIF.

      lt_billing_data = VALUE bapivbrk_t( FOR ls_lips IN lt_lips ( ref_doc    = ls_lips-vbeln
                                   ref_item   = ls_lips-posnr
                                   ref_doc_ca = 'J'  ) ).     " Delivery

      "********Check locks on deliveries*************
      "Wait for lock release
      DATA(lv_lock) = abap_true.
      LOOP AT it_deliveries INTO DATA(lv_delivery). "Check lock for each delivery
        lv_lock = abap_true.                        "Set initial lock as true.
        WHILE ( lv_lock EQ abap_true ).
          CALL FUNCTION 'SHP_ENQUEUE_EVVBLKE'           "Try to lock the delivery first. This will fail if the delivery is already locked
            EXPORTING
              mode_likp         = 'E'
              mandt             = sy-mandt
              vbeln             = lv_delivery
            EXCEPTIONS
              foreign_lock      = 1
              system_failure    = 2
              csl_no_connection = 3
              csl_inconsistency = 4
              OTHERS            = 5.
          IF sy-subrc EQ 0.                         "If locking is successful then no lock were there previously
            CALL FUNCTION 'SHP_DEQUEUE_EVVBLKE'     "Unlock the locked delivery
              EXPORTING
                mode_likp = 'E'
                mandt     = sy-mandt
                vbeln     = lv_delivery.
            lv_lock = abap_false.
          ELSE.
            WAIT UP TO 1 SECONDS.                   "If delivery is already locked then wait for 1 sec to release lock
          ENDIF.
        ENDWHILE.
      ENDLOOP.
*************************************************************
****************** Create Invoice ***************************
*************************************************************
      CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
        EXPORTING
          testrun       = abap_false
          posting       = 'C' "COMMIT WORK AND WAIT
        TABLES                         "
          billingdatain = lt_billing_data
          return        = et_bapiret2
          success       = lt_success.

      READ TABLE lt_success ASSIGNING FIELD-SYMBOL(<ls_success>) INDEX 1.
      IF sy-subrc <> 0 OR <ls_success>-bill_doc IS INITIAL.
        "Log error message
      ELSE.
        bill_doc = <ls_success>-bill_doc .
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~create_db_objects.
    DATA: ls_return TYPE bapiret2.
    IF it_pp_orders IS NOT INITIAL AND it_pp_req IS NOT INITIAL AND it_pp_req_item IS NOT INITIAL.
      READ TABLE it_pp_orders INTO DATA(ls_orders) INDEX 1.
      IF cl_rfm_st_pick_order=>gv_call_update_task IS NOT INITIAL.
        CALL FUNCTION 'RFM_ST_PICK_ORDER_MODIFY' IN UPDATE TASK
          EXPORTING
            it_pick_orders   = it_pp_orders
            it_pick_request  = it_pp_req
            it_pick_req_item = it_pp_req_item.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'S'.
        ls_return-number = '021'.
        ls_return-message_v1 = ls_orders-storepickuporder.
        SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
        IF 1 = 0. MESSAGE s021(rfm_st_pick_ord).ENDIF.
        APPEND ls_return TO et_return.
        CLEAR ls_return.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'S'.
        ls_return-number = '038'.
        LOOP AT it_pp_req INTO DATA(ls_pp_req_success).
          ls_return-message_v1 = ls_pp_req_success-storepickingrequest.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          APPEND ls_return TO et_return.
          CLEAR ls_return-message_v1.
        ENDLOOP.
      ELSE.
        DATA: lt_pp_orders   TYPE TABLE FOR CREATE i_storepickupordertp,
              lt_pp_req      TYPE TABLE FOR CREATE i_storepickingrequesttp,
              lt_pp_req_item TYPE TABLE FOR CREATE i_storepickingrequesttp\_items,
              ls_pp_req_item TYPE STRUCTURE FOR CREATE i_storepickingrequestitemtp.
        MOVE-CORRESPONDING it_pp_orders TO lt_pp_orders.
        MOVE-CORRESPONDING it_pp_req TO lt_pp_req.
        MOVE-CORRESPONDING it_pp_req_item TO lt_pp_req_item.
        lt_pp_req_item = CORRESPONDING #( it_pp_req MAPPING store = store
                                                                  storepickingrequest = storepickingrequest ).
        LOOP AT lt_pp_req_item ASSIGNING FIELD-SYMBOL(<fs_pp_req_item>).
          LOOP AT it_pp_req_item INTO DATA(ls_item) WHERE storepickingrequest = <fs_pp_req_item>-storepickingrequest.
            MOVE-CORRESPONDING ls_item TO ls_pp_req_item.
            APPEND ls_pp_req_item TO <fs_pp_req_item>-%target .
          ENDLOOP.
        ENDLOOP.

*       create store order first
        MODIFY ENTITIES OF i_storepickupordertp
        ENTITY i_storepickupordertp
        CREATE FROM lt_pp_orders
        FAILED DATA(lt_failed_orders)
        REPORTED DATA(lt_reported_orders).
        IF lt_failed_orders IS NOT INITIAL.

          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'E'.
          ls_return-number = '020'.
          ls_return-message_v1 = ls_orders-storepickuporder.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE e020(rfm_st_pick_ord).ENDIF.
          APPEND ls_return TO et_return.
        ELSE.
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'S'.
          ls_return-number = '021'.
          ls_return-message_v1 = ls_orders-storepickuporder.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE s021(rfm_st_pick_ord).ENDIF.
          APPEND ls_return TO et_return.
        ENDIF.
**        create header of picking request now.
*        MODIFY ENTITIES OF I_STOREPICKINGREQUESTTP
*        ENTITY I_STOREPICKINGREQUESTTP
*        CREATE FROM lt_pp_req
*        FAILED DATA(lt_failed_req)
*        REPORTED DATA(lt_reported_req).
**
**
**        create item of picking request now.
*        MODIFY ENTITIES OF I_STOREPICKINGREQUESTITEMTP
*        ENTITY I_STOREPICKINGREQUESTITEMTP
*        CREATE FROM lt_pp_req_item
*        FAILED DATA(lt_failed_req_item)
*        REPORTED DATA(lt_reported_req_item).




        MODIFY ENTITIES OF i_storepickingrequesttp  " name of root CDS view
        ENTITY i_storepickingrequesttp             " alias name
        CREATE FROM lt_pp_req
        CREATE BY \_items  FROM lt_pp_req_item
        FAILED   DATA(lt_failed)
        REPORTED DATA(lt_reported).
        IF lt_failed IS NOT INITIAL.
          ls_return-id = 'RFM_ST_PICK_REQ'.
          ls_return-type = 'E'.
          ls_return-number = '020'.
          ls_return-message_v1 = ls_orders-storepickuporder.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE e020(rfm_st_pick_req).ENDIF.
          APPEND ls_return TO et_return.
        ELSE.
          ls_return-id = 'RFM_ST_PICK_REQ'.
          ls_return-type = 'S'.
          ls_return-number = '038'.
          LOOP AT lt_pp_req INTO DATA(ls_pp_req_eml).
            ls_return-message_v1 = ls_pp_req_eml-storepickingrequest.
          ENDLOOP.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE s038(rfm_st_pick_req).ENDIF.
          APPEND ls_return TO et_return.
        ENDIF.
        IF lt_failed IS INITIAL.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~create_delivery.
    DATA: lt_order_items       TYPE STANDARD TABLE OF bapisditbos,
          lt_sales_order_items TYPE STANDARD TABLE OF bapidlvreftosalesorder,
          lt_return            TYPE TABLE OF bapiret2.
    DATA: ls_sales_order_items TYPE bapidlvreftosalesorder,
          lv_delivery          TYPE bapishpdelivnumb-deliv_numb,
          ls_return_msg        TYPE bapiret2.
    TYPES: tt_sales_order_items   TYPE TABLE OF bapidlvreftosalesorder WITH DEFAULT KEY.

    IF it_delv_inputs[] IS INITIAL.
      RETURN.
    ENDIF.
    READ TABLE it_delv_inputs INTO DATA(ls_delv_inputs) INDEX 1.
    IF sy-subrc EQ 0.
      SELECT SINGLE vstel FROM vbap
                     INTO @data(lv_shipping_point)
                     WHERE  vbeln = @ls_delv_inputs-sales_order AND posnr = @ls_delv_inputs-item.
    ENDIF.
    IF lv_shipping_point IS INITIAL.
      ls_return_msg-type = 'E'.
      ls_return_msg-message = 'Shipping point not found'.
      APPEND ls_return_msg TO et_return_msg.
      RETURN.
    ENDIF.
*    Get Sales order details
    READ TABLE it_delv_inputs INTO ls_delv_inputs INDEX 1.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BS01_SALESORDER_GETDETAIL'
        EXPORTING
          salesdocument = ls_delv_inputs-sales_order
        TABLES
          order_items   = lt_order_items
          return        = et_return_msg.

      "********Check the lock on sales order*****************
      DATA(lv_lock) = abap_true.
      WHILE ( lv_lock EQ abap_true ).
        CALL FUNCTION 'ENQUEUE_EVVBAKE'
          EXPORTING
            mode_vbak      = 'E'
            mandt          = sy-mandt
            vbeln          = ls_delv_inputs-sales_order
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'DEQUEUE_EVVBAKE'
            EXPORTING
              mode_vbak = 'E'
              mandt     = sy-mandt
              vbeln     = ls_delv_inputs-sales_order.
          lv_lock = abap_false.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDWHILE.

    ENDIF.

    CLEAR lt_sales_order_items.
    LOOP AT it_delv_inputs INTO ls_delv_inputs.
      READ TABLE lt_order_items INTO DATA(ls_order_items) WITH KEY doc_number = ls_delv_inputs-sales_order itm_number = ls_delv_inputs-item.
      IF sy-subrc EQ 0.
        ls_sales_order_items-ref_doc    =   ls_order_items-doc_number.
        ls_sales_order_items-ref_item   =   ls_order_items-itm_number.
        ls_sales_order_items-dlv_qty    =   ls_delv_inputs-delv_qty.
        ls_sales_order_items-sales_unit =   ls_order_items-sales_unit.
        APPEND ls_sales_order_items TO lt_sales_order_items.
      ENDIF.
    ENDLOOP.

*    Create Delivery against Sales Order items
    CLEAR: ev_deliv_numb,lt_return.
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      EXPORTING
        ship_point        = lv_shipping_point
*       due_date          = sy-datum
        no_dequeue        = abap_true
      IMPORTING
        delivery          = ev_deliv_numb
      TABLES
        sales_order_items = lt_sales_order_items
        created_items     = et_created_items
        return            = lt_return.
    APPEND LINES OF lt_return TO et_return_msg.
    READ TABLE lt_return WITH KEY type = if_xo_const_message=>error TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
*      Error in delivery
      RETURN.
    ENDIF.

*    Commit the BAPI transaction
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = ls_return_msg.
    IF ls_return_msg IS NOT INITIAL.
      APPEND ls_return_msg TO et_return_msg.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~delete_db_objects.
    DATA ls_return TYPE bapiret2.
    DATA lo_st_pkr_util TYPE REF TO if_rfm_st_pick_request_utility.
    IF it_st_order IS NOT INITIAL.
      IF cl_rfm_st_pick_order=>gv_call_update_task IS NOT INITIAL.
        CALL FUNCTION 'RFM_ST_PICK_ORDER_MODIFY' IN UPDATE TASK
          EXPORTING
            it_pick_order_delete = it_st_order.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'S'.
        ls_return-number = '024'.
        LOOP AT it_st_order INTO DATA(ls_st_order).
          ls_return-message_v1 = ls_st_order-storepickuporder.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE s024(rfm_st_pick_ord).ENDIF.
          APPEND ls_return TO et_return.
        ENDLOOP.
        lo_st_pkr_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).
        lo_st_pkr_util->read_st_pick_req(
          EXPORTING
            it_pp_orders  = it_st_order                 " Table type for structure RFM_ST_PICK_ORD
          IMPORTING
            et_return     = DATA(lt_return_pkr_read)                 " Table Type for BAPIRET2
            et_pp_request = DATA(lt_pp_req)                 " Table type of structure RFM_ST_PICK_REQ
        ).
        IF lt_return_pkr_read IS NOT INITIAL.
          APPEND LINES OF lt_return_pkr_read TO et_return.
        ELSE.
          CLEAR ls_return.
          ls_return-id = 'RFM_ST_PICK_REQ'.
          ls_return-type = 'S'.
          ls_return-number = '39'.
          LOOP AT lt_pp_req INTO DATA(ls_pp_req).
            ls_return-message_v1 = ls_pp_req-storepickingrequest.
            SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
            APPEND ls_return TO et_return.
          ENDLOOP.
        ENDIF.
      ELSE.

        DATA: lt_st_order TYPE TABLE FOR DELETE I_StorePickupOrderTP.
        MOVE-CORRESPONDING it_st_order TO lt_st_order.
        MODIFY ENTITIES OF I_StorePickupOrderTP
            ENTITY I_StorePickupOrderTP
            DELETE FROM lt_st_order
            FAILED   DATA(lt_failed_orders)
            REPORTED DATA(lt_reported_orders).
*        call store picking request BO method here to delete
        IF lt_failed_orders IS INITIAL.
*          MODIFY ENTITIES OF I_StorePickupOrderTP
*            ENTITY I_StorePickupOrderTP
*            DELETE FROM lt_st_order
*            FAILED   DATA(lt_failed_orders)
*            REPORTED DATA(lt_reported_orders).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~delete_delivery.

    DATA: ls_header_data    TYPE  bapiobdlvhdrchg,
          ls_header_control TYPE  bapiobdlvhdrctrlchg,
          ls_bapiret        TYPE bapiret2.
    IF iv_delivery IS INITIAL.
      RETURN.
    ENDIF.
    "Logic to check existing lock on delivery
    DATA(lv_lock) = abap_true.
    WHILE ( lv_lock EQ abap_true ).
      "Try to lock delivery first
      CALL FUNCTION 'SHP_ENQUEUE_EVVBLKE'
        EXPORTING
          mode_likp         = 'E'
          mandt             = sy-mandt
          vbeln             = iv_delivery
        EXCEPTIONS
          foreign_lock      = 1
          system_failure    = 2
          csl_no_connection = 3
          csl_inconsistency = 4
          OTHERS            = 5.
      IF sy-subrc EQ 0. "If we are able to lock the delivery then no lock exists on delivery before

        CALL FUNCTION 'SHP_DEQUEUE_EVVBLKE' "Unlock the locked delivery for further processing
          EXPORTING
            mode_likp = 'E'
            mandt     = sy-mandt
            vbeln     = iv_delivery.
        lv_lock = abap_false.
      ELSE.
        WAIT UP TO 1 SECONDS. "If locking on delivery fails then wait for 1 sec.
      ENDIF.
    ENDWHILE.
    "Set header data
    ls_header_data-deliv_numb = iv_delivery.
    "Set header control data
    ls_header_control-deliv_numb = iv_delivery.
    ls_header_control-dlv_del = abap_true.
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = ls_header_data
        header_control = ls_header_control
        delivery       = iv_delivery
      TABLES
        return         = rt_return.
    .
    READ TABLE rt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = ls_bapiret.
    IF ls_bapiret-type IS NOT INITIAL.
    APPEND ls_bapiret TO rt_return.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~delete_item_from_delivery.
    DATA: ls_header_data    TYPE  bapiobdlvhdrchg,
          ls_header_control TYPE  bapiobdlvhdrctrlchg,
          ls_bapiret        TYPE bapiret2.
    DATA: lt_item_data    TYPE TABLE OF bapiobdlvitemchg,
          lt_item_control TYPE TABLE OF bapiobdlvitemctrlchg,
          ls_item_data    TYPE bapiobdlvitemchg,
          ls_item_control TYPE bapiobdlvitemctrlchg.
    IF iv_delivery IS INITIAL OR iv_delv_item IS INITIAL.
      RETURN.
    ENDIF.
    "Logic to check existing lock on delivery
    DATA(lv_lock) = abap_true.
    WHILE ( lv_lock EQ abap_true ).
      "Try to lock delivery first
      CALL FUNCTION 'SHP_ENQUEUE_EVVBLKE'
        EXPORTING
          mode_likp         = 'E'
          mandt             = sy-mandt
          vbeln             = iv_delivery
        EXCEPTIONS
          foreign_lock      = 1
          system_failure    = 2
          csl_no_connection = 3
          csl_inconsistency = 4
          OTHERS            = 5.
      IF sy-subrc EQ 0. "If we are able to lock the delivery then no lock exists on delivery before

        CALL FUNCTION 'SHP_DEQUEUE_EVVBLKE' "Unlock the locked delivery for further processing
          EXPORTING
            mode_likp = 'E'
            mandt     = sy-mandt
            vbeln     = iv_delivery.
        lv_lock = abap_false.
      ELSE.
        WAIT UP TO 1 SECONDS. "If locking on delivery fails then wait for 1 sec.
      ENDIF.
    ENDWHILE.
    "Set header data
    ls_header_data-deliv_numb = iv_delivery.
    "Set header control data
    ls_header_control-deliv_numb = iv_delivery.
    "Set item data
    CLEAR: ls_item_data,lt_item_data.
    ls_item_data-deliv_numb = iv_delivery.
    ls_item_data-deliv_item = iv_delv_item.
    APPEND ls_item_data to lt_item_data.
    CLEAR: ls_item_control,lt_item_control.
    ls_item_control-deliv_numb = iv_delivery.
    ls_item_control-deliv_item = iv_delv_item.
    ls_item_control-del_item = abap_true.
    APPEND ls_item_control to lt_item_control.
*    ls_header_control-dlv_del = abap_true.
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = ls_header_data
        header_control = ls_header_control
        delivery       = iv_delivery
      TABLES
        item_data      = lt_item_data
        item_control   = lt_item_control
        return         = rt_return.
    .
    READ TABLE rt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = ls_bapiret.
    IF ls_bapiret-type IS NOT INITIAL.
      APPEND ls_bapiret TO rt_return.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~delete_st_pickorder_subsprod.

    DATA: ls_return        TYPE bapiret2,
          lv_subsprd_count    TYPE int3,
          lt_delete_sub_items TYPE rfm_st_pick_reqi_t.

    lv_subsprd_count = 0.

    IF iv_storepickuporder IS NOT INITIAL AND
       iv_requestedproduct IS NOT INITIAL AND
       iv_substitutedproduct IS NOT INITIAL.

** Get all picking requests for the order
      SELECT  storepickingrequest FROM rfm_st_pick_req INTO TABLE @DATA(lt_storepickingrequest)
                                                             WHERE store = @iv_store AND
                                                                   referencestorepickuporder = @iv_storepickuporder.
      IF lt_storepickingrequest IS NOT INITIAL.
** Get all picking request items for the picking requests
        SELECT * FROM rfm_st_pick_reqi INTO TABLE @DATA(lt_storepickingrequestitem) FOR ALL ENTRIES IN @lt_storepickingrequest
                                       WHERE store = @iv_store AND
                                             storepickingrequest = @lt_storepickingrequest-storepickingrequest.
        IF lt_storepickingrequestitem IS NOT INITIAL.
** Read Requested product
          READ TABLE lt_storepickingrequestitem INTO DATA(ls_requestedproduct) WITH KEY  product = iv_requestedproduct
                                                                                         storepkngreqitemissubstituted = 'X'.
          IF sy-subrc = 0.
            LOOP AT lt_storepickingrequestitem ASSIGNING FIELD-SYMBOL(<lfs_storepickingrequestitem>)
                                               WHERE storepickingrequest = ls_requestedproduct-storepickingrequest AND
                                            requestedstorepickingreqitem = ls_requestedproduct-storepickingrequestitem.
              lv_subsprd_count = lv_subsprd_count + 1.
            ENDLOOP.
          ENDIF.
        ENDIF.

** Read substituted product to be removed
*        READ TABLE lt_storepickingrequestitem INTO DATA(ls_substitutedproduct) WITH KEY storepickingrequest = ls_requestedproduct-storepickingrequest
*                                                                                      product = iv_substitutedproduct
*                                                                                      storepkngreqitemissubstituted = ''.
        LOOP AT lt_storepickingrequestitem INTO DATA(ls_substitutedproduct) WHERE  product = iv_substitutedproduct AND
                                                                                      storepkngreqitemissubstituted = ''.
          APPEND ls_substitutedproduct TO lt_delete_sub_items.
        ENDLOOP.

        IF sy-subrc = 0.
          DELETE  rfm_st_pick_reqi FROM TABLE lt_delete_sub_items.
*          DELETE FROM rfm_st_pick_reqi WHERE store = @ls_substitutedproduct-store AND
*                                             storepickingrequest = @ls_substitutedproduct-storepickingrequest AND
*                                             storepickingrequestitem = @ls_substitutedproduct-storepickingrequestitem.
          IF sy-subrc = 0.
            IF lv_subsprd_count = 1.
** if no substitute , then need to clear the flag
              UPDATE rfm_st_pick_reqi  SET storepkngreqitemissubstituted = ''
                                     WHERE store = @ls_requestedproduct-store AND
                                           storepickingrequest = @ls_requestedproduct-storepickingrequest AND
                                           storepickingrequestitem = @ls_requestedproduct-storepickingrequestitem.
** Get aggregated product details
              SELECT SINGLE * FROM i_storepickupordexcptnitemtp INTO @DATA(ls_pickupordexcptnitem)
                                                                WHERE store = @iv_store AND
                                                                storepickuporder = @iv_storepickuporder AND
                                                                retailstorerequestedproduct = @iv_requestedproduct AND
                                                                unitofmeasure = @iv_unitofmeasure.
** Get pickup order details
              SELECT  SINGLE * FROM rfm_st_pick_ord INTO @DATA(ls_storepickuporder)
                                                          WHERE store = @iv_store AND
                                                                storepickuporder = @iv_storepickuporder.
              IF sy-subrc = 0 AND ls_storepickuporder-nmbrofsubstituteditems GT 0.
** reducing the substituted product count
                DATA(lv_nmbrofsubstituteditems) = ls_storepickuporder-nmbrofsubstituteditems - 1.

                IF ls_pickupordexcptnitem-productpickedquantity = 0.
** Increased the count of unavailable product as substituted has been deleted
                  DATA(lv_nmbrofunavailableitems) = ls_storepickuporder-nmbrofunavailableitems + 1.
                  DATA(lv_nmbrofpartiallypickeditems) = ls_storepickuporder-nmbrofpartiallypickeditems.
                ELSE.
                  IF ls_pickupordexcptnitem-productpickedquantity <> ls_pickupordexcptnitem-requestedquantity.
** Increased the count of partially picked product as substituted has been deleted
                    lv_nmbrofpartiallypickeditems = ls_storepickuporder-nmbrofpartiallypickeditems + 1.
                    lv_nmbrofunavailableitems = ls_storepickuporder-nmbrofunavailableitems.
                  ENDIF.
                ENDIF.
                UPDATE rfm_st_pick_ord  SET nmbrofsubstituteditems = @lv_nmbrofsubstituteditems,
                                            nmbrofunavailableitems = @lv_nmbrofunavailableitems,
                                            nmbrofpartiallypickeditems = @lv_nmbrofpartiallypickeditems
                                      WHERE store   = @iv_store AND
                                            storepickuporder = @iv_storepickuporder.
              ENDIF.
            ENDIF.
            ls_return-id = 'RFM_ST_PICK_ORD'.
            ls_return-type = 'S'.
            ls_return-number = '026'.
            ls_return-message_v1 = ls_substitutedproduct-product.
            APPEND ls_return TO et_return.
            IF 1 = 2. MESSAGE e026(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
          ELSE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~delete_st_pick_order.
    DATA: ls_return     TYPE bapiret2,
          lt_pp_orders  TYPE rfm_st_pick_ord_t,
          lv_sales_area TYPE string.

    IF iv_st_order IS NOT INITIAL. " single order deletion
*       Get organizational data
      CALL METHOD me->if_rfm_st_pick_order_utility~get_vbak_single
        EXPORTING
          iv_vbeln = iv_st_order
        IMPORTING
          es_vbak  = DATA(ls_salesorderheader_single).
** Authorization Check at sales are and sales document type level
      IF me->if_rfm_st_pick_order_utility~check_authorization( i_activity = '06' i_auart = ls_salesorderheader_single-auart i_spart = ls_salesorderheader_single-spart
                                              i_vkorg = ls_salesorderheader_single-vkorg i_vtweg = ls_salesorderheader_single-vtweg ) = abap_false.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        ls_return-number = '016'.
        CONCATENATE ls_salesorderheader_single-vkorg '/' ls_salesorderheader_single-vtweg '/' ls_salesorderheader_single-spart INTO lv_sales_area.
        ls_return-message_v1 = ls_salesorderheader_single-vbeln.
        ls_return-message_v2 = lv_sales_area.
        ls_return-message_v3 = ls_salesorderheader_single-auart.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e016(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
        RETURN.
      ENDIF.
      DELETE FROM rfm_st_pick_ord WHERE storepickuporder = iv_st_order.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_return.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.
    IF it_st_order IS NOT INITIAL. " multiple order deletion at a time
      LOOP AT it_st_order INTO DATA(ls_pp_orders).
*       Get organizational data
        CALL METHOD me->if_rfm_st_pick_order_utility~get_vbak_single
          EXPORTING
            iv_vbeln = ls_pp_orders-storepickuporder
          IMPORTING
            es_vbak  = DATA(ls_salesorderheader).
** Authorization Check at sales are and sales document type level
        IF me->if_rfm_st_pick_order_utility~check_authorization( i_activity = '06' i_auart = ls_Salesorderheader-auart i_spart = ls_Salesorderheader-spart
                                                i_vkorg = ls_Salesorderheader-vkorg i_vtweg = ls_Salesorderheader-vtweg ) = abap_false.
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'E'.
          ls_return-number = '016'.
          CONCATENATE ls_salesorderheader-vkorg '/' ls_salesorderheader-vtweg '/' ls_salesorderheader-spart INTO lv_sales_area.
          ls_return-message_v1 = ls_salesorderheader-vbeln.
          ls_return-message_v2 = lv_sales_area.
          ls_return-message_v3 = ls_salesorderheader-auart.
          APPEND ls_return TO et_return.
          IF 1 = 2. MESSAGE e016(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
          CONTINUE.
        ENDIF.
        APPEND ls_pp_orders TO lt_pp_orders.
      ENDLOOP.
      DELETE rfm_st_pick_ord FROM TABLE lt_pp_orders.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_return.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~fetch_user_personalization.

    DATA: lt_persparm TYPE TABLE OF fpb_persparm,
          ls_persparm TYPE fpb_persparm,
          ls_return   TYPE if_rfm_st_pick_request=>ty_behv_return.

    SELECT * FROM fpb_persparm WHERE perskey EQ @sy-uname INTO TABLE @lt_persparm.   " Move this to utility class maybe??? this select reads the personalization data for printer , box label name and conetnt list name

    IF sy-subrc EQ 0.
      READ TABLE lt_persparm INTO DATA(ls_persparm_plant) WITH KEY fieldname = 'PLANT'.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM rfm_c_st_sc_site WHERE site = @ls_persparm_plant-low AND setting_id = '1020' AND ref_doc_type = '01' INTO @DATA(ls_rfm_c_st_sc_site).
        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM rtst_c_pp_basic INTO @es_app_settings WHERE retailstoreschemaid = @ls_rfm_c_st_sc_site-scheme_id
                                                                                    AND retailstoreschematype = @ls_rfm_c_st_sc_site-setting_id
                                                                                    AND ref_doc_type = '01'.
        ELSE.
          SELECT SINGLE * FROM rfm_c_st_scheme WHERE setting_id = '1020' AND is_standard IS NOT INITIAL AND ref_doc_type = '01' INTO  @DATA(ls_rfm_c_st_scheme). "#EC CI_NOORDER
          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM rtst_c_pp_basic INTO @es_app_settings WHERE retailstoreschemaid = @ls_rfm_c_st_scheme-scheme_id
                                                                                    AND retailstoreschematype = @ls_rfm_c_st_scheme-setting_id
                                                                                    AND ref_doc_type = '01'.
          ENDIF.
        ENDIF.
        READ TABLE lt_persparm INTO ls_persparm WITH KEY fieldname = 'PRINTER'.
        IF sy-subrc EQ 0.

          DATA(lv_cloud) =  cl_cos_utilities=>is_cloud( ).
          DATA(lv_storeid) = ls_persparm_plant-low.
          IF lv_cloud = abap_false.
            lv_storeid = '%' && lv_storeid && '%'.
            SELECT SINGLE * FROM tsp03d INTO @es_tsp03d WHERE pastandort LIKE @lv_storeid
                                                            AND name EQ @ls_persparm-low.
          ELSE.
            SELECT SINGLE * FROM pqprintqueue INTO @es_pqprintqueue WHERE location_id EQ @lv_storeid
                                                                          AND location_id_type EQ 'PLT'
                                                                          AND qname EQ @ls_persparm-low.
          ENDIF.
          ev_printer = ls_persparm-low.

        ENDIF.
        READ TABLE lt_persparm INTO ls_persparm WITH KEY fieldname = 'BOX_LABEL'.
        IF sy-subrc EQ 0.

          SELECT SINGLE * FROM aps_om_form_tmpl_header INTO @es_boxlabel WHERE name = @ls_persparm-low AND dataprovider = 'FDP_RFM_ST_PICK_ORD_BOX_LABELS_SRV'.

          ev_labelname = ls_persparm-low.

        ENDIF.
        READ TABLE lt_persparm INTO ls_persparm WITH KEY fieldname = 'CONTENT_LIST'.
        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM aps_om_form_tmpl_header INTO @es_contentlist WHERE name = @ls_persparm-low
                                                                                  AND dataprovider = 'FDP_RFM_ST_PICK_ORD_CNTNT_LIST_SRV'.

          ev_contentlist = ls_persparm-low.

        ENDIF.

      ENDIF.
    ELSE.
*           RAISE MESSAGE FOR PERSONALIZATION SETTINGS NOT MAINTAINED IN TABLE FPB_PERSPARM ITSELF IN PICK ORD MESSAGE Class.
      CLEAR ls_return.
      ls_return-id = 'RFM_RTST_PP'. ls_return-type = if_abap_behv_message=>severity-error.. ls_return-number = '012'.
      IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'E' NUMBER '004'. ENDIF.
      APPEND ls_return TO et_return.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_entity_set.

    CALL METHOD iv_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = et_entity_data
        et_field_property = et_field_property.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_likp.

    IF it_deliveries IS NOT INITIAL.

      DATA(so_delv) = VALUE rseloption( FOR ls_delv IN it_deliveries ( sign = 'I' option = 'EQ' low = ls_delv ) ).
      IF so_delv IS NOT INITIAL.
        SELECT vbeln,wbstk FROM likp INTO CORRESPONDING FIELDS OF TABLE @et_deliveries WHERE vbeln IN @so_delv.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  method IF_RFM_ST_PICK_ORDER_UTILITY~GET_LIKP_DELV.
     TYPES:BEGIN OF lty_vbeln,
            Sign(1)   TYPE c,
            option(2) TYPE c,
            low       TYPE vbak-vbeln,
            high      TYPE vbak-vbeln,
          END OF lty_vbeln.
    TYPES: ltt_vbeln TYPE STANDARD TABLE OF lty_vbeln WITH DEFAULT KEY.
    DATA: lt_vbeln  TYPE TABLE OF lty_vbeln.
    lt_vbeln = VALUE ltt_vbeln( FOR ls_vbeln IN it_sales_order
                                (
                                 Sign = 'I'
                                 option = 'EQ'
                                 low = ls_vbeln ) ).

    IF iv_delivery_id IS NOT INITIAL.
      SELECT SINGLE kostk, pdstk
       INTO CORRESPONDING FIELDS OF @es_likp
       FROM likp
       WHERE vbeln = @iv_delivery_id.
    ENDIF.
    IF it_sales_order IS NOT INITIAL.
      SELECT hd~vbeln, hd~wbstk , it~vgbel FROM likp AS hd INNER JOIN lips AS it ON hd~vbeln = it~vbeln INTO CORRESPONDING FIELDS OF TABLE @et_likp  WHERE vgbel IN @lt_vbeln.
    ENDIF.
  endmethod.


  METHOD if_rfm_st_pick_order_utility~get_lips.

    IF iv_vbeln IS NOT INITIAL.
      SELECT vbeln FROM lips INTO TABLE et_deliveries WHERE vgbel = iv_vbeln.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_lips_additional_fields.


    IF iv_vbeln IS NOT INITIAL.

      SELECT vbeln,posnr,lfimg,lgmng,vgbel,vgpos,meins,werks,matnr FROM lips
       INTO CORRESPONDING FIELDS OF TABLE @et_deliveries
       WHERE vgbel = @iv_vbeln.
      gt_handover_data = et_deliveries.

    ENDIF.

  ENDMETHOD.


  method IF_RFM_ST_PICK_ORDER_UTILITY~GET_LIPS_DELV.
     if iv_delivery_id is not initial.
    SELECT * FROM lips
    INTO CORRESPONDING FIELDS OF TABLE @et_lips
    WHERE vbeln = @iv_delivery_id.
    ENDIF.
  endmethod.


  METHOD if_rfm_st_pick_order_utility~get_mara_array.
    DATA: lt_mara  TYPE STANDARD TABLE OF mara,
          lt_pre03 TYPE pre03_tab.
    IF it_pre03 IS NOT INITIAL.
      MOVE-CORRESPONDING it_pre03 TO lt_pre03.
      CALL FUNCTION 'MARA_ARRAY_READ'
*        EXPORTING
*          KZRFB                      = ' '
*          SPERRMODUS                 = ' '
*          STD_SPERRMODUS             = ' '
*        IMPORTING
*          RETC                       =
        TABLES
          ipre03               = lt_pre03
          mara_tab             = lt_mara
        EXCEPTIONS
          enqueue_mode_changed = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        APPEND LINES OF lt_mara TO et_mara.
      ENDIF.


    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order_utility~get_message_for_entity.

    iv_access->get_messages(
  EXPORTING
   iv_init    = abap_true
  IMPORTING
   et_message = et_message
   es_error   = es_error ).
  ENDMETHOD.
ENDCLASS.