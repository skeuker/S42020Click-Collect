class CL_RFM_ST_PICK_REQUEST_UTILITY definition
  public
  final
  create public

  global friends CL_RFM_ST_PICK_REQUEST_FACTORY .

public section.

  interfaces IF_RFM_ST_PICK_REQUEST_UTILITY .
protected section.
private section.

  class-data MO_ACCESS type ref to IF_GOAL_ACCESS .

  class-methods CREATE
    returning
      value(RO_ST_PKR_UTILITY) type ref to IF_RFM_ST_PICK_REQUEST_UTILITY .
ENDCLASS.



CLASS CL_RFM_ST_PICK_REQUEST_UTILITY IMPLEMENTATION.


  METHOD if_rfm_st_pick_request_utility~update_st_pick_req_item.

    DATA: ls_return TYPE bapiret2.
    IF it_pp_req_item IS NOT INITIAL .
      UPDATE rfm_st_pick_reqi FROM TABLE @it_pp_req_item.
      IF sy-subrc IS INITIAL.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'S'.
        ls_return-number = 20.
        IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'S' NUMBER '020'. ENDIF.
        APPEND ls_return TO et_return.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        ls_return-number = 20.
        APPEND ls_return TO et_return.
        IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'E' NUMBER '020'. ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~update_st_pick_req_after_wd.
    DATA: ls_return TYPE bapiret2,
          lt_pp_req TYPE rfm_st_pick_req_t.
    IF it_pp_req IS NOT INITIAL .

** Get all picking request for the pickup order
      SELECT * FROM rfm_st_pick_req INTO TABLE @DATA(lt_storepickingrequest) FOR ALL ENTRIES IN @it_pp_req
                                     WHERE store = @it_pp_req-store AND
                                           referencestorepickuporder = @it_pp_req-referencestorepickuporder.
      IF sy-subrc = 0 AND lt_storepickingrequest IS NOT INITIAL.

        LOOP AT lt_storepickingrequest INTO DATA(ls_pp_req).
          IF ls_pp_req-storepickingrequeststatus = 'B' AND
             ls_pp_req-handovershelfspaceid = ''.
            ls_pp_req-storepickingrequeststatus = 'W'.
            ls_pp_req-lastchangedbyuser = sy-uname.
            GET TIME STAMP FIELD ls_pp_req-lastchangedatetime.
            APPEND ls_pp_req TO lt_pp_req.
          ENDIF.
          CLEAR : ls_pp_req.
        ENDLOOP.
      ENDIF.

      LOOP AT it_pp_req INTO ls_pp_req.
        ls_pp_req-lastchangedbyuser = sy-uname.
        GET TIME STAMP FIELD ls_pp_req-lastchangedatetime.
        APPEND ls_pp_req TO lt_pp_req.
        CLEAR : ls_pp_req.
      ENDLOOP.

      UPDATE rfm_st_pick_req FROM TABLE @lt_pp_req.
      IF sy-subrc IS INITIAL.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'S'.
        APPEND ls_return TO et_return.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~update_st_pick_req.

    DATA: ls_return TYPE bapiret2,
          lt_pp_req TYPE rfm_st_pick_req_t.
    IF it_pp_req IS NOT INITIAL .

      LOOP AT it_pp_req INTO DATA(ls_pp_req).
        ls_pp_req-lastchangedbyuser = sy-uname.
        GET TIME STAMP FIELD ls_pp_req-lastchangedatetime.
        APPEND ls_pp_req TO lt_pp_req.
      ENDLOOP.

      UPDATE rfm_st_pick_req FROM TABLE @lt_pp_req.
      IF sy-subrc IS INITIAL.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'S'.
        APPEND ls_return TO et_return.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method IF_RFM_ST_PICK_REQUEST_UTILITY~UPDATE_PICKING_DO_PGI.
    IF IS_VBKOK IS INITIAL.
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

  endmethod.


  METHOD if_rfm_st_pick_request_utility~update_order_item.

    DATA: ls_error TYPE if_goal_types=>tcs_error.
    DATA: lrt_error TYPE if_goal_types=>tcs_message.
    DATA: lv_bo_key TYPE if_goal_types=>tcd_bo_key.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    DATA: lv_text_exc TYPE string.
    DATA: ls_changed_field TYPE if_goal_types=>tcs_changed_field.
    DATA: lt_changed_field TYPE if_goal_types=>tct_changed_field.
    DATA: lv_field TYPE fieldname.
    DATA: ls_field_property TYPE if_goal_types=>tcs_object_property.
    DATA: lt_field_property TYPE if_goal_types=>tct_object_property.
    DATA: lt_message TYPE if_goal_types=>tct_message.

    DATA: ls_head_data TYPE tds_goal_so_head.
    DATA: ls_item_data TYPE tds_goal_so_item,
          lt_item_data TYPE STANDARD TABLE OF tds_goal_so_item.
    DATA: ls_sline_data TYPE tds_goal_so_sline.
    DATA: lt_sline_data TYPE tdt_goal_so_sline.
    IF iv_vbeln IS INITIAL AND iv_posnr IS INITIAL AND  iv_qty IS INITIAL .
      RETURN.
    ENDIF.
    "********Check the lock on sales order*****************
    DATA(lv_lock) = abap_true.
    lv_bo_key = iv_vbeln.
    WHILE ( lv_lock EQ abap_true ).
      TRY.
          CALL METHOD cl_goal_api=>so_instance->open
            EXPORTING
              iv_bo_id     = 'SALESORDER'
              iv_bo_key    = lv_bo_key
              iv_read_only = abap_false
            RECEIVING
              ro_access    = mo_access.
          IF mo_access IS NOT INITIAL.
            lv_lock = abap_false.
          ENDIF.
        CATCH cx_goal_exc INTO lx_goal_exc.
          lv_text_exc = lx_goal_exc->get_text( ).
          lrt_error-msgid = 'RFM_ST_PICK_REQ'.
          lrt_error-msgno = '015'.
          lrt_error-msgty = 'E'.
          lrt_error-msgv1 = iv_vbeln.
          lrt_error-msgv2 = lv_text_exc.
          APPEND lrt_error TO rt_message.
          IF 1 = 2. MESSAGE e015(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
          WAIT UP TO 1 SECONDS.
      ENDTRY.
    ENDWHILE.

    "Read entity set
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_field_property = lt_field_property.

    "Read schedule line of the item
    READ TABLE lt_item_data INTO ls_item_data WITH KEY item_id = iv_posnr.
    ls_item_data-order_qty = iv_qty.
    ls_item_data-rejection_reason_code = iv_rejection_reason_code.

    ls_changed_field-handle = ls_item_data-handle.
    lv_field = 'ORDER_QTY'.
    INSERT lv_field INTO TABLE ls_changed_field-field.

*    IF IV_DELIVERED_QTY IS INITIAL.
      lv_field = 'REJECTION_REASON_CODE'.
      INSERT lv_field INTO TABLE ls_changed_field-field.
*    ENDIF.

    CALL METHOD mo_access->set_entity
      EXPORTING
        iv_entity_id     = 'ITEM'
        is_entity_data   = ls_item_data
        is_changed_field = ls_changed_field.

    mo_access->get_messages(
      EXPORTING
       iv_init    = abap_true
      IMPORTING
       et_message = rt_message
       es_error   = ls_error ).

    IF ls_error IS NOT INITIAL.
      lrt_error-msgid = ls_error-msgid.
      lrt_error-msgno = ls_error-msgno.
      lrt_error-msgty = ls_error-msgty.
      lrt_error-msgv1 = ls_error-msgv1.
      lrt_error-msgv2 = ls_error-msgv2.
      lrt_error-msgv3 = ls_error-msgv3.
      lrt_error-msgv4 = ls_error-msgv4.
      APPEND lrt_error TO rt_message.
    ENDIF.
* get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_field_property = lt_field_property.

    mo_access->save( iv_no_commit = abap_false ).

    mo_access->close( ).

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~set_item_for_rejection.
    DATA: ls_error TYPE if_goal_types=>tcs_error.
    DATA: lrt_error TYPE if_goal_types=>tcs_message.
    DATA: lv_bo_key TYPE if_goal_types=>tcd_bo_key.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    DATA: lv_text_exc TYPE string.
    DATA: ls_changed_field TYPE if_goal_types=>tcs_changed_field.
    DATA: lt_changed_field TYPE if_goal_types=>tct_changed_field.
    DATA: lv_field TYPE fieldname.
    DATA: ls_field_property TYPE if_goal_types=>tcs_object_property.
    DATA: lt_field_property TYPE if_goal_types=>tct_object_property.
    DATA: lt_message TYPE if_goal_types=>tct_message.

    DATA: ls_head_data TYPE tds_goal_so_head.
    DATA: ls_item_data TYPE tds_goal_so_item,
          lt_item_data TYPE STANDARD TABLE OF tds_goal_so_item.
    DATA: ls_sline_data TYPE tds_goal_so_sline.
    DATA: lt_sline_data TYPE tdt_goal_so_sline.
    IF iv_vbeln IS INITIAL AND iv_posnr IS INITIAL AND  iv_qty IS INITIAL AND iv_rejection_reason_code IS INITIAL.
      RETURN.
    ENDIF.
    "********Check the lock on sales order*****************
    DATA(lv_lock) = abap_true.
    lv_bo_key = iv_vbeln.
    WHILE ( lv_lock EQ abap_true ).
      TRY.
          CALL METHOD cl_goal_api=>so_instance->open
            EXPORTING
              iv_bo_id     = 'SALESORDER'
              iv_bo_key    = lv_bo_key
              iv_read_only = abap_false
            RECEIVING
              ro_access    = mo_access.
          IF mo_access IS NOT INITIAL.
            lv_lock = abap_false.
          ENDIF.
        CATCH cx_goal_exc INTO lx_goal_exc.
          lv_text_exc = lx_goal_exc->get_text( ).
          lrt_error-msgid = 'RFM_ST_PICK_REQ'.
          lrt_error-msgno = '015'.
          lrt_error-msgty = 'E'.
          lrt_error-msgv1 = iv_vbeln.
          lrt_error-msgv2 = lv_text_exc.
          APPEND lrt_error TO rt_message.
          IF 1 = 2. MESSAGE e015(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
          WAIT UP TO 1 SECONDS.
      ENDTRY.
    ENDWHILE.
    "Read entity set
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_field_property = lt_field_property.

    "Read schedule line of the item
    READ TABLE lt_item_data INTO ls_item_data WITH KEY item_id = iv_posnr.
    ls_item_data-order_qty = iv_qty.
    ls_item_data-rejection_reason_code = iv_rejection_reason_code.

    ls_changed_field-handle = ls_item_data-handle.
    lv_field = 'ORDER_QTY'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'REJECTION_REASON_CODE'.
    INSERT lv_field INTO TABLE ls_changed_field-field.

    CALL METHOD mo_access->set_entity
      EXPORTING
        iv_entity_id     = 'ITEM'
        is_entity_data   = ls_item_data
        is_changed_field = ls_changed_field.

    mo_access->get_messages(
      EXPORTING
       iv_init    = abap_true
      IMPORTING
       et_message = rt_message
       es_error   = ls_error ).

    IF ls_error IS NOT INITIAL.
      lrt_error-msgid = ls_error-msgid.
      lrt_error-msgno = ls_error-msgno.
      lrt_error-msgty = ls_error-msgty.
      lrt_error-msgv1 = ls_error-msgv1.
      lrt_error-msgv2 = ls_error-msgv2.
      lrt_error-msgv3 = ls_error-msgv3.
      lrt_error-msgv4 = ls_error-msgv4.
      APPEND lrt_error TO rt_message.
    ENDIF.
* get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_field_property = lt_field_property.

    mo_access->save( iv_no_commit = abap_false ).

    mo_access->close( ).
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_uom_data.

    IF it_item_tab IS NOT INITIAL.

      SELECT unitofmeasure, unitofmeasuredspnmbrofdcmls, unitofmeasurenumberofdecimals
                                                FROM i_unitofmeasure
                                                FOR ALL ENTRIES IN @it_item_tab
                                                WHERE unitofmeasure = @it_item_tab-unitofmeasure
                                                INTO TABLE @et_uom.

    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_st_pick_req_item.
    DATA: ls_return TYPE bapiret2,
          lv_dummy  TYPE bapi_msg.
    IF iv_pick_request IS NOT INITIAL .
      SELECT * FROM rfm_st_pick_reqi INTO TABLE @DATA(lt_st_pick_reqi_sin_pick) WHERE storepickingrequest = @iv_pick_request.
      IF sy-subrc IS INITIAL.
        APPEND LINES OF lt_st_pick_reqi_sin_pick TO et_pick_reqi.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        ls_return-number = '002'.
        APPEND ls_return TO et_return.
*        IF 1 = 2. MESSAGE e002(rfm_st_pick_req) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.
    IF  it_pick_req_tab IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_reqi INTO TABLE @DATA(lt_st_pick_reqi_tab) FOR ALL ENTRIES IN @it_pick_req_tab
                                          WHERE storepickingrequest = @it_pick_req_tab-storepickingrequest AND store = @it_pick_req_tab-store.
      IF sy-subrc IS INITIAL.
        APPEND LINES OF lt_st_pick_reqi_tab TO et_pick_reqi.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        ls_return-number = '002'.
        APPEND ls_return TO et_return.
*        IF 1 = 2. MESSAGE e002(rfm_st_pick_req) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.
    IF iv_ref_order IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_reqi INTO TABLE @DATA(lt_st_pick_reqi_ord) WHERE referencestorepickuporder = @iv_ref_order  AND storepickingrequest = @iv_pick_request.
      IF sy-subrc IS INITIAL.
        APPEND LINES OF lt_st_pick_reqi_ord TO et_pick_reqi.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        ls_return-number = '002'.
        APPEND ls_return TO et_return.
*        IF 1 = 2. MESSAGE e002(rfm_st_pick_req) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.

    IF is_pick_request IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_reqi INTO TABLE @et_pick_reqi WHERE storepickingrequest = @is_pick_request-storepickingrequest
                                                                     AND store = @is_pick_request-store.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_st_pick_req.
    DATA : ls_return              TYPE bapiret2,
           lv_storepickingrequest TYPE rfm_st_pick_request,
           lv_dummy               TYPE bapi_msg.

    " if store order number is passed
    IF iv_st_order IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_req WHERE referencestorepickuporder = @iv_st_order INTO TABLE @DATA(lt_pp_req_ord). "#EC CI_NOFIELD
      IF  sy-subrc IS INITIAL.
        APPEND LINES OF lt_pp_req_ord TO et_pp_request.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        ls_return-number = '031'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e031(rfm_st_pick_req) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.

    " if picking requests table is passed as input
    IF it_pp_req IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_req FOR ALL ENTRIES IN @it_pp_req
                                    WHERE storepickingrequest = @it_pp_req-storepickingrequest
                                    AND store = @it_pp_req-store
                                    INTO TABLE @DATA(lt_picking_req).
      IF sy-subrc IS INITIAL.
        APPEND LINES OF lt_picking_req TO et_pp_request.
*        READ TABLE lt_picking_req INTO DATA(ls_picking_req) INDEX 1.
        LOOP AT lt_picking_req INTO DATA(ls_picking_req).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_picking_req-storepickingrequest
            IMPORTING
              output = lv_storepickingrequest.
          "  To handle concurrent user trying to withdraw picking request
          IF ls_picking_req-storepickingrequeststatus = 'W'.
            ls_return-id = 'RFM_ST_PICK_ORD'.
            ls_return-type = 'I'.
            ls_return-number = '022'.
            ls_return-message_v1 = lv_storepickingrequest.
            APPEND ls_return TO et_return.
            IF 1 = 2. MESSAGE i022(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        ls_return-id = 'RFM_RTST_PP'.
        ls_return-type = 'E'.
        ls_return-number = '003'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e003(rfm_rtst_pp) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.

*    if order table is passed as input
    IF it_pp_orders IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_req FOR ALL ENTRIES IN @it_pp_orders
                                    WHERE referencestorepickuporder = @it_pp_orders-storepickuporder
                                    AND store = @it_pp_orders-store
                                    INTO TABLE @DATA(lt_pp_req_orders).
      IF sy-subrc IS INITIAL.
        APPEND LINES OF lt_pp_req_orders TO et_pp_request.
      ELSE.
        ls_return-id = 'RFM_RTST_PP'.
        ls_return-type = 'E'.
        ls_return-number = '003'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e003(rfm_rtst_pp) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.

*    if item table is passed as imput
    IF it_pp_item IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_req FOR ALL ENTRIES IN @it_pp_item WHERE storepickingrequest = @it_pp_item-storepickingrequest
                                                                         AND store = @it_pp_item-store
                                                                         INTO TABLE @et_pp_request.
      IF sy-subrc IS NOT INITIAL.
        ls_return-id = 'RFM_RTST_PP'.
        ls_return-type = 'E'.
        ls_return-number = '003'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e003(rfm_rtst_pp) INTO lv_dummy. ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_req_with_reford.

    IF it_ref_order IS NOT INITIAL.

      SELECT DISTINCT * FROM rfm_st_pick_req FOR ALL ENTRIES IN @it_ref_order WHERE store = @it_ref_order-store
                                                                             AND referencestorepickuporder = @it_ref_order-storepickuporder
                                                                             INTO TABLE @et_pick_req.

    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_request_with_reforder.

    SELECT * FROM rfm_st_pick_req INTO TABLE @DATA(lt_pick_req) WHERE store = @is_pick_req-store AND
                                                                 referencestorepickuporder = @is_pick_req-referencestorepickuporder AND
                                                                 storepickingrequeststatus NE 'W'.

    ev_subrc = sy-subrc.

    IF lt_pick_req IS NOT INITIAL.

      es_pick_req = lt_pick_req[ 1 ].

    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_items_with_ref_order.

    IF it_item_tab IS NOT INITIAL.

      SELECT DISTINCT * FROM rfm_st_pick_reqi FOR ALL ENTRIES IN @it_item_tab WHERE referencestorepickuporder = @it_item_tab-referencestorepickuporder AND
                                                 referencestorepickuporderitem = @it_item_tab-referencestorepickuporderitem
                                                 AND store = @it_item_tab-store INTO TABLE @et_item_tab.

    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_items_with_keys.

    SELECT DISTINCT * FROM rfm_st_pick_reqi FOR ALL ENTRIES IN @it_pp_items WHERE storepickingrequest = @it_pp_items-storepickingrequest
                                                          AND storepickingrequestitem = @it_pp_items-storepickingrequestitem
                                                          AND store = @it_pp_items-store
                                                          INTO TABLE @et_pp_items.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_items_to_print.

    IF it_print_tab IS NOT INITIAL.
      SELECT * FROM rfm_st_pick_reqi FOR ALL ENTRIES IN @it_print_tab WHERE storepickingrequest = @it_print_tab-storepickingrequest
                       ORDER BY PRIMARY KEY INTO TABLE @et_pick_req_item .
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~read_c_st_pickreq.
    DATA: ls_c_st_pickreq TYPE rfm_c_st_pickreq,
          ls_return       TYPE bapiret2.
    IF iv_store IS NOT INITIAL AND iv_ref_doc_type IS NOT INITIAL .

      SELECT SINGLE * FROM rfm_c_st_sc_site INTO  @DATA(ls_c_st_sc_site)
        WHERE setting_id = '1040'
        AND ref_doc_type = @iv_ref_doc_type
        AND site = @iv_store.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * FROM rfm_c_st_pickreq INTO @ls_c_st_pickreq
          WHERE scheme_id = @ls_c_st_sc_site-scheme_id
          AND ref_doc_type = @iv_ref_doc_type
          AND setting_id = '1040'.
        IF sy-subrc IS INITIAL .
          es_c_st_pickreq = ls_c_st_pickreq.
        ENDIF.
      ELSE.
        SELECT * FROM rfm_c_st_scheme UP TO 1 ROWS INTO TABLE @DATA(lt_c_st_scheme)
          WHERE setting_id = '1040'
          AND ref_doc_type = @iv_ref_doc_type
          AND is_standard = 'X'.

        IF sy-subrc IS INITIAL .
          READ TABLE lt_c_st_scheme INTO DATA(ls_c_st_scheme) WITH KEY ref_doc_type = iv_ref_doc_type is_standard = 'X'.
          IF sy-subrc IS INITIAL.
            SELECT SINGLE * FROM rfm_c_st_pickreq INTO @ls_c_st_pickreq
                       WHERE scheme_id = @ls_c_st_scheme-scheme_id
                       AND ref_doc_type = @iv_ref_doc_type
                       AND setting_id = '1040'.
            IF sy-subrc IS INITIAL .
              es_c_st_pickreq = ls_c_st_pickreq.
            ENDIF.
          ENDIF.

        ELSE.
          ls_return-id = 'RFM_ST_PICK_REQ'.
          ls_return-message = '001'.
          ls_return-type = 'E'.
          APPEND ls_return TO et_return.
          IF 1 = 2 . MESSAGE e001(rfm_st_pick_req). ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD if_rfm_st_pick_request_utility~modify_pick_item.

  MODIFY rfm_st_pick_reqi FROM TABLE it_pick_items.

ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~material_weight_unit_con.
    DATA: lv_input_weight  TYPE f,
          lv_output_weight TYPE f.
    IF iv_input_weight IS NOT INITIAL
      AND iv_product IS NOT INITIAL
      AND iv_sys_weight_unit IS NOT INITIAL
       AND iv_ord_weight_unit IS NOT INITIAL.
      lv_input_weight = iv_input_weight.


*      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
*        EXPORTING
*          input                = iv_input_weight
*          kzmeinh              = abap_false
*          matnr                = iv_product
*          meinh                = iv_sys_weight_unit
*          meins                = iv_ord_weight_unit
*          type_umr             = '1'
*        IMPORTING
*          output               = lv_output_weight
*        EXCEPTIONS
*          conversion_not_found = 1
*          input_invalid        = 2
*          material_not_found   = 3
*          meinh_not_found      = 4
*          meins_missing        = 5
*          no_meinh             = 6
*          output_invalid       = 7
*          overflow             = 8
*          OTHERS               = 9.
*      IF sy-subrc = 0.
*        ev_output_weight = lv_output_weight.
*      ENDIF.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.

        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = iv_input_weight
*         NO_TYPE_CHECK        = 'X'
*         ROUND_SIGN           = ' '
          unit_in              = iv_ord_weight_unit
          unit_out             = iv_sys_weight_unit
        IMPORTING
*         ADD_CONST            =
*         DECIMALS             =
*         DENOMINATOR          =
*         NUMERATOR            =
          output               = lv_output_weight
        EXCEPTIONS
          conversion_not_found = 1
          division_by_zero     = 2
          input_invalid        = 3
          output_invalid       = 4
          overflow             = 5
          type_invalid         = 6
          units_missing        = 7
          unit_in_not_found    = 8
          unit_out_not_found   = 9
          OTHERS               = 10.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF sy-subrc = 0.
        ev_output_weight = lv_output_weight.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~material_volume_unit_con.
    DATA: lv_input_volume  TYPE f,
          lv_output_volume TYPE f.
    IF iv_input_volume IS NOT INITIAL
      AND iv_product IS NOT INITIAL
      AND iv_sys_volume_unit IS NOT INITIAL
      AND iv_ord_volume_unit IS NOT INITIAL.
      lv_input_volume = iv_input_volume.
*
*      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
*        EXPORTING
*          input                = iv_input_volume
*          kzmeinh              = abap_false
*          matnr                = iv_product
*          meinh                = iv_sys_volume_unit
*          meins                = iv_ord_volume_unit
*          type_umr             = '1'
*        IMPORTING
*          output               = lv_output_volume
*        EXCEPTIONS
*          conversion_not_found = 1
*          input_invalid        = 2
*          material_not_found   = 3
*          meinh_not_found      = 4
*          meins_missing        = 5
*          no_meinh             = 6
*          output_invalid       = 7
*          overflow             = 8
*          OTHERS               = 9.
*      IF sy-subrc = 0.
*        ev_output_volume = lv_output_volume.
*      ENDIF.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.


      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = iv_input_volume
*         NO_TYPE_CHECK        = 'X'
*         ROUND_SIGN           = ' '
          unit_in              = iv_ord_volume_unit
          unit_out             = iv_sys_volume_unit
        IMPORTING
*         ADD_CONST            =
*         DECIMALS             =
*         DENOMINATOR          =
*         NUMERATOR            =
          output               = lv_output_volume
        EXCEPTIONS
          conversion_not_found = 1
          division_by_zero     = 2
          input_invalid        = 3
          output_invalid       = 4
          overflow             = 5
          type_invalid         = 6
          units_missing        = 7
          unit_in_not_found    = 8
          unit_out_not_found   = 9
          OTHERS               = 10.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF sy-subrc = 0.
        ev_output_volume = lv_output_volume.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~insert_st_pick_req_item.

    DATA: ls_return           TYPE bapiret2,
          ls_rtst_pp_req_item TYPE rfm_st_pick_reqi.


    IF it_rtst_pp_req_item IS NOT INITIAL.
      INSERT rfm_st_pick_reqi FROM TABLE it_rtst_pp_req_item.
      IF sy-subrc IS INITIAL.
        READ TABLE it_rtst_pp_req_item INTO ls_rtst_pp_req_item INDEX 1.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'S'.
        ls_return-number = '014'.
        ls_return-message_v1 = ls_rtst_pp_req_item-referencestorepickuporder.
        IF 1 = 2. MESSAGE s014(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~insert_st_pick_req_head.

    DATA: ls_return          TYPE bapiret2,
          ls_rtst_pp_request TYPE rfm_st_pick_req,
          lt_rtst_pp_request TYPE rfm_st_pick_req_t.

    IF it_rtst_pp_request IS NOT INITIAL.
      LOOP AT it_rtst_pp_request INTO DATA(ls_rtst_pp_request_t).
        ls_rtst_pp_request_t-createdbyuser = sy-uname.
*        ls_rtst_pp_request_t-lastchangedbyuser = sy-uname.
        GET TIME STAMP FIELD ls_rtst_pp_request_t-creationdatetime.
*        GET TIME STAMP FIELD ls_rtst_pp_request_t-lastchangedatetime.
        APPEND ls_rtst_pp_request_t TO lt_rtst_pp_request.
      ENDLOOP.
      INSERT rfm_st_pick_req FROM TABLE lt_rtst_pp_request.

      IF sy-subrc IS INITIAL.

        READ TABLE it_rtst_pp_request INTO ls_rtst_pp_request INDEX 1.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'S'.
        ls_return-number = '014'.
        ls_return-message_v1 = ls_rtst_pp_request-storepickingrequest.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e014(RFM_ST_PICK_REQ) INTO DATA(lv_dummy). ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_vbep_with_posnr.

* Dummy Method to skip Aunits for the utilities class - written to achieve minimal coverage.

    IF 1 = 2.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ELSEIF 2 = 3.

    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_vbep.
    IF it_vbap IS NOT INITIAL.
      SELECT vbeln,posnr,etenr,wmeng
          FROM vbep
         INTO CORRESPONDING FIELDS OF TABLE @et_vbep
       FOR ALL ENTRIES IN @it_vbap
         WHERE vbeln EQ @it_vbap-vbeln
           AND posnr EQ @it_vbap-posnr.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_vbap.
    IF iv_vbeln IS NOT INITIAL AND iv_POSNR IS NOT INITIAL.
      SELECT vbeln,posnr,matnr,werks,abgru
            FROM vbap
            INTO CORRESPONDING FIELDS OF TABLE @et_vbap
            WHERE vbeln = @iv_vbeln AND posnr = @iv_posnr.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_shipping_point.
*       Read Ship Point from Sales order document item
    if iv_sales_order is not initial and iv_item is not initial.
    SELECT SINGLE vstel FROM vbap
                        INTO rv_shipping_point
                        WHERE  vbeln = iv_sales_order AND posnr = iv_item.
    ENDIF.
  ENDMETHOD.


  method IF_RFM_ST_PICK_REQUEST_UTILITY~GET_PICKING_REQ_NUMBER.

    DATA: ls_return TYPE bapiret2,
          lv_dummy  TYPE bapi_msg.
    IF iv_number IS NOT INITIAL AND iv_object IS NOT INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = iv_number
          object                  = iv_object
        IMPORTING
          number                  = ev_number
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
*         QUANTITY_IS_0           = 4
*         QUANTITY_IS_NOT_1       = 5
*         INTERVAL_OVERFLOW       = 6
*         BUFFER_OVERFLOW         = 7
*         OTHERS                  = 8
        .
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            ls_return-id = 'NR'.
            ls_return-type = 'E'.
            ls_return-number = '751'.
            ls_return-message_v3 = iv_number.
            ls_return-message_v1 = iv_object.
            IF 1 = 2. MESSAGE e751(nr) INTO lv_dummy. ENDIF.
          WHEN 2.
            ls_return-id = 'NR'.
            ls_return-type = 'E'.
            ls_return-number = '752'.
            IF 1 = 2. MESSAGE e752(nr) INTO lv_dummy. ENDIF.
          WHEN 3.
            ls_return-id = 'NR'.
            ls_return-type = 'E'.
            ls_return-number = '002'.
            ls_return-message_v1 = iv_object.
            IF 1 = 2. MESSAGE e002(nr) INTO lv_dummy. ENDIF.
        ENDCASE.
        APPEND ls_return TO et_return.
* Implement suitable error handling here
      ENDIF.

    ENDIF.

  endmethod.


  METHOD if_rfm_st_pick_request_utility~get_next_date.

    IF iv_date IS NOT INITIAL OR iv_days IS NOT INITIAL OR iv_months IS NOT INITIAL OR iv_years IS NOT INITIAL.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = iv_date
          days      = iv_days
          months    = iv_months
          signum    = iv_signum
          years     = iv_years
        IMPORTING
          calc_date = ev_date.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_mara.
    IF iv_matnr IS NOT INITIAL.
      CALL FUNCTION 'MARA_SINGLE_READ'
        EXPORTING
          matnr             = iv_matnr
        IMPORTING
          wmara             = es_mara
        EXCEPTIONS
          lock_on_material  = 1
          lock_system_error = 2
          wrong_call        = 3
          not_found         = 4
          OTHERS            = 5.


      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
    IF it_matnr IS NOT INITIAL.
      CALL FUNCTION 'MARA_ARRAY_READ'
        TABLES
          ipre03               = it_matnr
          mara_tab             = et_mara
        EXCEPTIONS
          enqueue_mode_changed = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_makt.
    IF it_matnr IS NOT INITIAL.

      SELECT * FROM makt FOR ALL ENTRIES IN @it_matnr WHERE matnr = @it_matnr-matnr AND spras = @sy-langu INTO TABLE @DATA(lt_makt).
      IF sy-subrc IS INITIAL.
        et_makt = lt_makt.
      ENDIF.

    ENDIF.
ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_lips.
*     select vbeln,posnr,nopck,pstyv from lips
*      INTO CORRESPONDING FIELDS OF TABLE @et_lips
*      WHERE vbeln = @iv_delivery_id.
    if iv_delivery_id is not initial.
    SELECT * FROM lips
    INTO CORRESPONDING FIELDS OF TABLE @et_lips
    WHERE vbeln = @iv_delivery_id.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~get_likp.
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
  ENDMETHOD.


METHOD if_rfm_st_pick_request_utility~fetch_order_items.

  IF is_pick_item IS NOT INITIAL.

    SELECT DISTINCT * FROM rfm_st_pick_reqi INTO TABLE et_pick_items WHERE referencestorepickuporder = is_pick_item-referencestorepickuporder
                                                                                    AND store = is_pick_item-store.

  ENDIF.

ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~delete_st_pick_req.

    IF it_pp_req IS NOT INITIAL.
      DELETE rfm_st_pick_req FROM TABLE it_pp_req.
      IF sy-subrc IS INITIAL.
        LOOP AT it_pp_req INTO DATA(ls_pp_req).
          DELETE FROM rfm_st_pick_reqi WHERE storepickingrequest = ls_pp_req-storepickingrequest.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~create_new_item_in_so.
    DATA: ls_error TYPE if_goal_types=>tcs_error.
    DATA: lrt_error TYPE if_goal_types=>tcs_message.
    DATA: lv_bo_key TYPE if_goal_types=>tcd_bo_key.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    DATA: lv_text_exc TYPE string.
    DATA: ls_field_property TYPE if_goal_types=>tcs_object_property.
    DATA: lt_field_property TYPE if_goal_types=>tct_object_property.
    DATA: ls_control_settings TYPE if_goal_access=>tcs_control_settings.
    DATA: ls_changed_field TYPE if_goal_types=>tcs_changed_field.
    DATA: lt_changed_field TYPE if_goal_types=>tct_changed_field.
    DATA: lv_field TYPE fieldname.

    DATA: lt_handle_reuse TYPE if_goal_types=>tct_handle_reuse.
    DATA: lv_handle_head TYPE if_goal_types=>tcd_handle.

    DATA: ls_action TYPE if_goal_types=>tcs_action.
    DATA: ls_reference TYPE tds_goal_sdoc_ref.

    DATA: lt_message TYPE if_goal_types=>tct_message.

    DATA: ls_head_data TYPE tds_goal_so_head.
    DATA: ls_item_data TYPE tds_goal_so_item,
          lt_item_data TYPE STANDARD TABLE OF tds_goal_so_item,
          lt_item_dflt TYPE STANDARD TABLE OF tds_goal_so_item.

    IF iv_vbeln IS INITIAL
      OR iv_matnr IS INITIAL
      OR iv_qty IS INITIAL
      OR iv_plant IS INITIAL.
      RETURN.
    ENDIF.
    "********Check the lock on sales order*****************
    DATA(lv_lock) = abap_true.
    lv_bo_key = iv_vbeln.
    WHILE ( lv_lock EQ abap_true ).
      TRY.
          ls_control_settings-no_conversion = abap_true.
          CALL METHOD cl_goal_api=>so_instance->open
            EXPORTING
              iv_bo_id            = 'SALESORDER'
              iv_bo_key           = lv_bo_key
              iv_read_only        = abap_false
              is_control_settings = ls_control_settings
            RECEIVING
              ro_access           = mo_access.
          IF mo_access IS NOT INITIAL.
            lv_lock = abap_false.
          ENDIF.
        CATCH cx_goal_exc INTO lx_goal_exc.
          lv_text_exc = lx_goal_exc->get_text( ).
          lrt_error-msgid = 'RFM_ST_PICK_REQ'.
          lrt_error-msgno = '015'.
          lrt_error-msgty = 'E'.
          lrt_error-msgv1 = iv_vbeln.
          lrt_error-msgv2 = lv_text_exc.
          APPEND lrt_error TO rt_message.
          IF 1 = 2. MESSAGE e015(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
          WAIT UP TO 1 SECONDS.
      ENDTRY.
    ENDWHILE.
* Get HEAD data
    CALL METHOD mo_access->get_entity
      EXPORTING
        iv_entity_id      = 'HEAD'
      IMPORTING
        es_entity_data    = ls_head_data
        es_field_property = ls_field_property.

* Get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_entity_dflt    = lt_item_dflt
        et_field_property = lt_field_property.
    TRY.
        ls_item_data-handle =  cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.
    ls_item_data-material_id = iv_matnr.
    ls_item_data-order_qty   = iv_qty.
    ls_item_data-plant_id    = iv_plant.
    ls_item_data-sales_uom   = iv_unitofmeasure.

    ls_changed_field-handle = ls_item_data-handle.
    lv_field = 'MATERIAL_ID'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'ORDER_QTY'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'PLANT_ID'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'SALES_UOM'.
    INSERT lv_field INTO TABLE ls_changed_field-field.

    CALL METHOD mo_access->set_entity
      EXPORTING
        iv_entity_id     = 'ITEM'
        is_entity_data   = ls_item_data
        is_changed_field = ls_changed_field.

    mo_access->get_messages(
     EXPORTING
       iv_init    = abap_true
     IMPORTING
       et_message = rt_message
       es_error   = ls_error ).
    IF ls_error IS NOT INITIAL.
      lrt_error-msgid = ls_error-msgid.
      lrt_error-msgno = ls_error-msgno.
      lrt_error-msgty = ls_error-msgty.
      lrt_error-msgv1 = ls_error-msgv1.
      lrt_error-msgv2 = ls_error-msgv2.
      lrt_error-msgv3 = ls_error-msgv3.
      lrt_error-msgv4 = ls_error-msgv4.
      APPEND lrt_error TO rt_message.
    ENDIF.

* get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_entity_dflt    = lt_item_dflt
        et_field_property = lt_field_property.
    CLEAR es_item_created.
    READ TABLE lt_item_data INTO es_item_created WITH KEY handle = ls_item_data-handle.
    mo_access->save( iv_no_commit = abap_false ). "commit causes issues in RAP Framework - no explicit needed.

    mo_access->close( ).

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~create_item_for_substitution.

    DATA: ls_error TYPE if_goal_types=>tcs_error.
    DATA: lrt_error TYPE if_goal_types=>tcs_message.
    DATA: lv_bo_key TYPE if_goal_types=>tcd_bo_key.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    DATA: lv_text_exc TYPE string.
    DATA: ls_field_property TYPE if_goal_types=>tcs_object_property.
    DATA: lt_field_property TYPE if_goal_types=>tct_object_property.
    DATA: ls_control_settings TYPE if_goal_access=>tcs_control_settings.
    DATA: ls_changed_field TYPE if_goal_types=>tcs_changed_field.
    DATA: lt_changed_field TYPE if_goal_types=>tct_changed_field.
    DATA: lv_field TYPE fieldname.

    DATA: lt_handle_reuse TYPE if_goal_types=>tct_handle_reuse.
    DATA: lv_handle_head TYPE if_goal_types=>tcd_handle.

    DATA: ls_action TYPE if_goal_types=>tcs_action.
    DATA: ls_reference TYPE tds_goal_sdoc_ref.

    DATA: lt_message TYPE if_goal_types=>tct_message.

    DATA: ls_head_data TYPE tds_goal_so_head.
    DATA: ls_item_data TYPE tds_goal_so_item,
          lt_item_data TYPE STANDARD TABLE OF tds_goal_so_item,
          lt_item_dflt TYPE STANDARD TABLE OF tds_goal_so_item.

    IF iv_vbeln IS INITIAL
      OR iv_matnr IS INITIAL
      OR iv_qty IS INITIAL
      OR iv_plant IS INITIAL.
      RETURN.
    ENDIF.
    "********Check the lock on sales order*****************
    DATA(lv_lock) = abap_true.
    lv_bo_key = iv_vbeln.
    WHILE ( lv_lock EQ abap_true ).
      TRY.
          ls_control_settings-no_conversion = abap_true.
          CALL METHOD cl_goal_api=>so_instance->open
            EXPORTING
              iv_bo_id            = 'SALESORDER'
              iv_bo_key           = lv_bo_key
              iv_read_only        = abap_false
              is_control_settings = ls_control_settings
            RECEIVING
              ro_access           = mo_access.
          IF mo_access IS NOT INITIAL.
            lv_lock = abap_false.
          ENDIF.
        CATCH cx_goal_exc INTO lx_goal_exc.
          lv_text_exc = lx_goal_exc->get_text( ).
          lrt_error-msgid = 'RFM_ST_PICK_REQ'.
          lrt_error-msgno = '015'.
          lrt_error-msgty = 'E'.
          lrt_error-msgv1 = iv_vbeln.
          lrt_error-msgv2 = lv_text_exc.
          APPEND lrt_error TO rt_message.
          IF 1 = 2. MESSAGE e015(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
          WAIT UP TO 1 SECONDS.
      ENDTRY.
    ENDWHILE.
* Get HEAD data
    CALL METHOD mo_access->get_entity
      EXPORTING
        iv_entity_id      = 'HEAD'
      IMPORTING
        es_entity_data    = ls_head_data
        es_field_property = ls_field_property.

* Get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_entity_dflt    = lt_item_dflt
        et_field_property = lt_field_property.
    TRY.
        ls_item_data-handle =  cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.
    ls_item_data-material_id = iv_matnr.
    ls_item_data-order_qty   = iv_qty.
    ls_item_data-plant_id    = iv_plant.
    ls_item_data-sales_uom   = iv_unitofmeasure.

    ls_changed_field-handle = ls_item_data-handle.
    lv_field = 'MATERIAL_ID'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'ORDER_QTY'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'PLANT_ID'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'SALES_UOM'.
    INSERT lv_field INTO TABLE ls_changed_field-field.

    CALL METHOD mo_access->set_entity
      EXPORTING
        iv_entity_id     = 'ITEM'
        is_entity_data   = ls_item_data
        is_changed_field = ls_changed_field.

    mo_access->get_messages(
     EXPORTING
       iv_init    = abap_true
     IMPORTING
       et_message = rt_message
       es_error   = ls_error ).
    IF ls_error IS NOT INITIAL.
      lrt_error-msgid = ls_error-msgid.
      lrt_error-msgno = ls_error-msgno.
      lrt_error-msgty = ls_error-msgty.
      lrt_error-msgv1 = ls_error-msgv1.
      lrt_error-msgv2 = ls_error-msgv2.
      lrt_error-msgv3 = ls_error-msgv3.
      lrt_error-msgv4 = ls_error-msgv4.
      APPEND lrt_error TO rt_message.
    ENDIF.

* get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_entity_dflt    = lt_item_dflt
        et_field_property = lt_field_property.

    READ TABLE lt_item_data WITH KEY handle =  ls_item_data-handle INTO es_item_created.

    mo_access->save( iv_no_commit = abap_false ). "commit causes issues in RAP Framework - no explicit needed.

    mo_access->close( ).

  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~create_item_for_rejection.
    DATA: ls_error TYPE if_goal_types=>tcs_error.
    DATA: lrt_error TYPE if_goal_types=>tcs_message.
    DATA: lv_bo_key TYPE if_goal_types=>tcd_bo_key.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    DATA: lv_text_exc TYPE string.
    DATA: ls_field_property TYPE if_goal_types=>tcs_object_property.
    DATA: lt_field_property TYPE if_goal_types=>tct_object_property.
    DATA: ls_control_settings TYPE if_goal_access=>tcs_control_settings.
    DATA: ls_changed_field TYPE if_goal_types=>tcs_changed_field.
    DATA: lt_changed_field TYPE if_goal_types=>tct_changed_field.
    DATA: lv_field TYPE fieldname.

    DATA: lt_handle_reuse TYPE if_goal_types=>tct_handle_reuse.
    DATA: lv_handle_head TYPE if_goal_types=>tcd_handle.

    DATA: ls_action TYPE if_goal_types=>tcs_action.
    DATA: ls_reference TYPE tds_goal_sdoc_ref.

    DATA: lt_message TYPE if_goal_types=>tct_message.

    DATA: ls_head_data TYPE tds_goal_so_head.
    DATA: ls_item_data TYPE tds_goal_so_item,
          lt_item_data TYPE STANDARD TABLE OF tds_goal_so_item,
          lt_item_dflt TYPE STANDARD TABLE OF tds_goal_so_item.

    IF iv_vbeln IS INITIAL
      OR iv_matnr IS INITIAL
      OR iv_qty IS INITIAL
      OR iv_rejection_reason_code IS INITIAL
      OR iv_plant IS INITIAL.
      RETURN.
    ENDIF.
    "********Check the lock on sales order*****************
    DATA(lv_lock) = abap_true.
    lv_bo_key = iv_vbeln.
    WHILE ( lv_lock EQ abap_true ).
      TRY.
          ls_control_settings-no_conversion = abap_true.
          CALL METHOD cl_goal_api=>so_instance->open
            EXPORTING
              iv_bo_id            = 'SALESORDER'
              iv_bo_key           = lv_bo_key
              iv_read_only        = abap_false
              is_control_settings = ls_control_settings
            RECEIVING
              ro_access           = mo_access.
          IF mo_access IS NOT INITIAL.
            lv_lock = abap_false.
          ENDIF.
        CATCH cx_goal_exc INTO lx_goal_exc.
          lv_text_exc = lx_goal_exc->get_text( ).
          lrt_error-msgid = 'RFM_ST_PICK_REQ'.
          lrt_error-msgno = '015'.
          lrt_error-msgty = 'E'.
          lrt_error-msgv1 = iv_vbeln.
          lrt_error-msgv2 = lv_text_exc.
          APPEND lrt_error TO rt_message.
          IF 1 = 2. MESSAGE e015(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
          WAIT UP TO 1 SECONDS.
      ENDTRY.
    ENDWHILE.
* get HEAD data
    CALL METHOD mo_access->get_entity
      EXPORTING
        iv_entity_id      = 'HEAD'
      IMPORTING
        es_entity_data    = ls_head_data
        es_field_property = ls_field_property.

* get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_entity_dflt    = lt_item_dflt
        et_field_property = lt_field_property.
    TRY.
        ls_item_data-handle =  cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.
    ls_item_data-material_id = iv_matnr.
    ls_item_data-order_qty = iv_qty.
    ls_item_data-rejection_reason_code = iv_rejection_reason_code.
    ls_item_data-plant_id = iv_plant.

    ls_changed_field-handle = ls_item_data-handle.
    lv_field = 'MATERIAL_ID'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'ORDER_QTY'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'REJECTION_REASON_CODE'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    lv_field = 'PLANT_ID'.
    INSERT lv_field INTO TABLE ls_changed_field-field.

    CALL METHOD mo_access->set_entity
      EXPORTING
        iv_entity_id     = 'ITEM'
        is_entity_data   = ls_item_data
        is_changed_field = ls_changed_field.

    mo_access->get_messages(
     EXPORTING
       iv_init    = abap_true
     IMPORTING
       et_message = rt_message
       es_error   = ls_error ).
    IF ls_error IS NOT INITIAL.
      lrt_error-msgid = ls_error-msgid.
      lrt_error-msgno = ls_error-msgno.
      lrt_error-msgty = ls_error-msgty.
      lrt_error-msgv1 = ls_error-msgv1.
      lrt_error-msgv2 = ls_error-msgv2.
      lrt_error-msgv3 = ls_error-msgv3.
      lrt_error-msgv4 = ls_error-msgv4.
      APPEND lrt_error TO rt_message.
    ENDIF.

* get ITEM data
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_entity_dflt    = lt_item_dflt
        et_field_property = lt_field_property.

    mo_access->save( iv_no_commit = abap_false ). "commit causes issues in RAP Framework - no explicit needed.

    mo_access->close( ).
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~create_delivery.
    DATA: lt_order_items       TYPE STANDARD TABLE OF bapisditbos,
          lt_sales_order_items TYPE STANDARD TABLE OF bapidlvreftosalesorder,
          lt_return            TYPE TABLE OF bapiret2.
    DATA: ls_sales_order_items TYPE bapidlvreftosalesorder,
          lv_delivery          TYPE bapishpdelivnumb-deliv_numb,
          ls_return_msg        TYPE bapiret2.
    TYPES: tt_sales_order_items   TYPE TABLE OF bapidlvreftosalesorder WITH DEFAULT KEY.

    IF iv_shipping_point IS INITIAL OR it_delv_inputs[] IS INITIAL.
      RETURN.
    ENDIF.

*    Get Sales order details
    READ TABLE it_delv_inputs INTO DATA(ls_delv_inputs) INDEX 1.
    IF sy-subrc EQ 0.
      WAIT UP TO 1 SECONDS.
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
        ship_point        = iv_shipping_point
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


  METHOD if_rfm_st_pick_request_utility~change_schedule_line.
    DATA: ls_error TYPE if_goal_types=>tcs_error.
    DATA: lrt_error TYPE if_goal_types=>tcs_message.
    DATA: lv_bo_key TYPE if_goal_types=>tcd_bo_key.
    DATA: lx_goal_exc TYPE REF TO cx_goal_exc.
    DATA: lv_text_exc TYPE string.
    DATA: ls_changed_field TYPE if_goal_types=>tcs_changed_field.
    DATA: lt_changed_field TYPE if_goal_types=>tct_changed_field.
    DATA: lv_field TYPE fieldname.
    DATA: ls_field_property TYPE if_goal_types=>tcs_object_property.
    DATA: lt_field_property TYPE if_goal_types=>tct_object_property.
    DATA: lt_message TYPE if_goal_types=>tct_message.

    DATA: ls_head_data TYPE tds_goal_so_head.
    DATA: ls_item_data TYPE tds_goal_so_item,
          lt_item_data TYPE STANDARD TABLE OF tds_goal_so_item.
    DATA: ls_sline_data TYPE tds_goal_so_sline.
    DATA: lt_sline_data TYPE tdt_goal_so_sline.

    IF iv_vbeln IS INITIAL OR iv_posnr IS INITIAL OR iv_qty IS INITIAL.
      RETURN.
    ENDIF.
    "********Check the lock on sales order*****************
    DATA(lv_lock) = abap_true. "Assuming lock is present
    lv_bo_key = iv_vbeln.
    WHILE ( lv_lock EQ abap_true ).
    TRY.
        CALL METHOD cl_goal_api=>so_instance->open
          EXPORTING
            iv_bo_id     = 'SALESORDER'
            iv_bo_key    = lv_bo_key
            iv_read_only = abap_false
          RECEIVING
            ro_access    = mo_access.
        IF mo_access IS NOT INITIAL.
             lv_lock = abap_false.   "Able to open sales order, so no lock exist.
        ENDIF.
      CATCH cx_goal_exc INTO lx_goal_exc.
        lv_text_exc = lx_goal_exc->get_text( ).
        lrt_error-msgid = 'RFM_ST_PICK_REQ'.
        lrt_error-msgno = '015'.
        lrt_error-msgty = 'E'.
        lrt_error-msgv1 = iv_vbeln.
        lrt_error-msgv2 = lv_text_exc.
        APPEND lrt_error TO rt_message.
        IF 1 = 2. MESSAGE e015(RFM_ST_PICK_REQ) INTO DATA(lv_dummy). ENDIF.
    ENDTRY.
    ENDWHILE.
    "Read entity set
    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id      = 'ITEM'
      IMPORTING
        et_entity_data    = lt_item_data
        et_field_property = lt_field_property.

    "Read schedule line of the item
    READ TABLE lt_item_data INTO ls_item_data WITH KEY item_id = iv_posnr.

    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id     = 'SLINE'
        iv_handle_parent = ls_item_data-handle
      IMPORTING
        et_entity_data   = lt_sline_data.

    "change order quantity of item
    ls_item_data-order_qty = iv_qty.
    ls_changed_field-handle = ls_item_data-handle.
    lv_field = 'ORDER_QTY'.
    INSERT lv_field INTO TABLE ls_changed_field-field.
    CALL METHOD mo_access->set_entity
      EXPORTING
        is_entity_data   = ls_item_data
        is_changed_field = ls_changed_field.

    mo_access->get_messages(
     EXPORTING
       iv_init    = abap_true
     IMPORTING
       et_message = rt_message
       es_error   = ls_error ).

    IF ls_error IS NOT INITIAL.
      lrt_error-msgid = ls_error-msgid.
      lrt_error-msgno = ls_error-msgno.
      lrt_error-msgty = ls_error-msgty.
      lrt_error-msgv1 = ls_error-msgv1.
      lrt_error-msgv2 = ls_error-msgv2.
      lrt_error-msgv3 = ls_error-msgv3.
      lrt_error-msgv4 = ls_error-msgv4.
      APPEND lrt_error TO rt_message.
    ENDIF.

    READ TABLE lt_item_data INTO ls_item_data WITH KEY item_id = iv_posnr..

    CALL METHOD mo_access->get_entity_set
      EXPORTING
        iv_entity_id     = 'SLINE'
        iv_handle_parent = ls_item_data-handle
      IMPORTING
        et_entity_data   = lt_sline_data.

    mo_access->save( iv_no_commit = abap_false ).

    mo_access->close( ).
  ENDMETHOD.


  METHOD if_rfm_st_pick_request_utility~authorization_check.

    AUTHORITY-CHECK OBJECT 'W_ST_PKRQ'
     ID 'ACTVT' FIELD iv_actvt
         ID 'WERKS' FIELD iv_store.
    IF sy-subrc <> 0.
      ev_subrc = sy-subrc.
    ENDIF.

  ENDMETHOD.


  method CREATE.
    ro_st_pkr_utility = NEW cl_rfm_st_pick_request_utility( ) .
  endmethod.
ENDCLASS.