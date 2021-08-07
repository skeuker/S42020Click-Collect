class CL_RFM_ST_PICK_REQUEST definition
  public
  final
  create private

  global friends CL_RFM_ST_PICK_REQUEST_FACTORY .

public section.

  interfaces IF_RFM_ST_PICK_REQUEST .

  constants MV_MSG_ID type MSGID value 'RFM_ST_PICK_REQ' ##NO_TEXT.
  constants MV_ITEM_REJECTED type RFM_ST_PICK_REQ_ITEM_STATUS value 'O' ##NO_TEXT.
  constants MV_ITEM_SHORTPICK type RFM_ST_PICK_REQ_ITEM_STATUS value 'B' ##NO_TEXT.
  constants MV_ITEM_COMPLETE type RFM_ST_PICK_REQ_ITEM_STATUS value 'A' ##NO_TEXT.
  constants MV_RQST_NOTPICKED type RFM_ST_REQUEST_STATUS value 'O' ##NO_TEXT.
  constants MV_RQST_INPROCESS type RFM_ST_REQUEST_STATUS value 'A' ##NO_TEXT.
  constants MV_RQST_COMPLETE type RFM_ST_REQUEST_STATUS value 'B' ##NO_TEXT.
  constants MV_ORDR_PICK_ONTRACK type RFM_ST_ORDER_PICKING_STATUS value '01' ##NO_TEXT.
  constants MV_ORDR_PICK_INPRCSS type RFM_ST_ORDER_PICKING_STATUS value '02' ##NO_TEXT.
  constants MV_ORDR_PICK_CREATED type RFM_ST_ORDER_PICKING_STATUS value '03' ##NO_TEXT.
  constants MV_ORDR_OPEN type RFM_ST_ORDER_OVERALL_STATUS value 'O' ##NO_TEXT.
  constants MV_ORDR_INPROCESS type RFM_ST_ORDER_OVERALL_STATUS value 'P' ##NO_TEXT.
  constants MV_ORDR_COMPLETE type RFM_ST_ORDER_OVERALL_STATUS value 'C' ##NO_TEXT.
  class-data MV_REASON type ABGRU_VA .
  class-data GV_ACTION_FROM_API type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !IO_APPLICATION_LOG type ref to CL_RTST_APPLICATION_LOG optional .
protected section.

  data GO_APPLICATION_LOG type ref to CL_RTST_APPLICATION_LOG .
private section.

  data MO_STR_ORD type ref to IF_RFM_ST_PICK_ORDER .
  data MO_ST_PKR_UTIL type ref to IF_RFM_ST_PICK_REQUEST_UTILITY .
  class-data MS_C_PP_STORE type RFM_C_PICK_ORD .
  class-data MS_C_ST_PICKREQ type RFM_C_ST_PICKREQ .
  data MO_ST_PICK_HDLR type ref to IF_RFM_ST_PICK_REQ_HNDLR .

  class-methods CREATE
    returning
      value(RO_ST_PKR) type ref to IF_RFM_ST_PICK_REQUEST .
  methods UPDATE_PICKING_ITEM
    importing
      !IT_PP_REQ type RFM_ST_PICK_REQ_T optional
      !IT_PP_REQ_ITEM type RFM_ST_PICK_REQI_T optional
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
  methods BEGIN_PICKING
    importing
      !IT_RTST_PKR_PRINT type RTST_PKR_PRINT_T optional
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
  methods ASSIGN_STORAGE
    importing
      !IT_PP_REQ type RFM_ST_PICK_REQ_T
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
  methods SET_OBJECTS .
  methods WITHDRAW_PICKING
    importing
      !IT_PP_REQ type RFM_ST_PICK_REQ_T
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods FINISH_PICKING .
  methods DELETE_ST_PICK_REQUEST
    importing
      !IT_PP_REQ type RFM_ST_PICK_REQ_T optional
      !IV_ST_ORDER type RFM_ST_PICKUP_ORDER optional
      !IT_PP_ORDERS type RFM_ST_PICK_ORD_T optional
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods CREATE_ST_PICK_REQUEST
    importing
      !IT_ST_PICK_HEAD type RFM_ST_PICK_REQ_T
      !IT_ST_PICK_ITEM type RFM_ST_PICK_REQI_T
    exporting
      !ET_RETURN type TT_BAPIRET2
      !ET_ST_PICK_HEAD type RFM_ST_PICK_REQ_T
      !ET_ST_PICK_ITEM type RFM_ST_PICK_REQI_T .
  methods TAKEOVER_PICK_REQUEST
    importing
      !IT_TAKEOVER_REQ type RFM_ST_PICK_REQ_T
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
  methods RELEASE_PICK_REQUEST
    importing
      !IT_RELEASE_REQ type RFM_ST_PICK_REQ_T
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
  methods TRANSFER_PICK_REQUEST
    importing
      !IT_TRANSFER_REQ type RFM_ST_PICK_REQ_T
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
  methods PROCESS_GTIN
    importing
      !IV_SCANNED_GTIN type EAN11
    exporting
      !EV_WEIGHT type BRGEW_AP
      !EV_WEIGHT_UNIT type GEWEI
      !EV_PRICE type ENDBP
      !EV_CURRENCY_UNIT type WAERK
      !EV_GTIN type EAN11 .
  methods CREATE_PICKING_ITEM
    importing
      !IT_PP_REQ_ITEM type RFM_ST_PICK_REQI_T
    exporting
      !ET_RETURN type IF_RFM_ST_PICK_REQUEST~TT_BEHV_RETURN .
ENDCLASS.



CLASS CL_RFM_ST_PICK_REQUEST IMPLEMENTATION.


  METHOD withdraw_picking.
    DATA: lt_pp_orders TYPE rfm_st_pick_ord_t,
          ls_pp_orders TYPE rfm_st_pick_ord,
          ls_uw_pp_req TYPE rfm_st_pick_req,
          ls_return    TYPE bapiret2.
    FIELD-SYMBOLS <fs_pp_req> TYPE rfm_st_pick_req.
    IF it_pp_req IS NOT INITIAL.
      READ TABLE it_pp_req INTO DATA(ls_pp_req) INDEX 1.

      CALL METHOD mo_st_pkr_util->authorization_check
        EXPORTING
          iv_store = ls_pp_req-store
          iv_actvt = '75'
        IMPORTING
          ev_subrc = DATA(lv_subrc).

      IF lv_subrc NE 0.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = 'E'.
        ls_return-number = '036'.
        APPEND ls_return TO et_return.
        IF 1 = 2. MESSAGE e036(rfm_st_pick_req) INTO DATA(lv_dummy). ENDIF.
        RETURN.
      ENDIF.

      CALL METHOD mo_st_pkr_util->read_st_pick_req
        EXPORTING
          it_pp_req     = it_pp_req
        IMPORTING
          et_return     = DATA(lt_return_read)
          et_pp_request = DATA(lt_pp_req).

      IF lt_return_read IS INITIAL.

        READ TABLE lt_pp_req ASSIGNING <fs_pp_req> WITH KEY store                     = ls_pp_req-store
                                                            storepickingrequest     = ls_pp_req-storepickingrequest
                                                            referencestorepickuporder = ls_pp_req-referencestorepickuporder.
        IF sy-subrc = 0.
          IF  <fs_pp_req>-storepickingrequeststatus = 'W'.
            ls_return-id = 'RFM_ST_PICK_ORD'.
            ls_return-type = 'I'.
            ls_return-number = '022'.
            ls_return-message_v1 = ls_pp_req-storepickingrequest.
            APPEND ls_return TO et_return.
            IF 1 = 2. MESSAGE i022(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
          ELSE.
            <fs_pp_req>-storepickingrequeststatus = ls_pp_req-storepickingrequeststatus.
          ENDIF.
        ENDIF.

*        CALL METHOD mo_st_pkr_util->update_st_pick_req
*          EXPORTING
*            it_pp_req = lt_pp_req
*          IMPORTING
*            et_return = DATA(lt_return_req).

        CALL METHOD mo_st_pkr_util->update_st_pick_req_after_wd
          EXPORTING
            it_pp_req = lt_pp_req
          IMPORTING
            et_return = DATA(lt_return_req).

        IF lt_return_req IS NOT INITIAL.

          READ TABLE lt_return_req INTO DATA(ls_return_req) INDEX 1.

          IF sy-subrc IS INITIAL.

            IF ls_return_req-type = 'S'.
              ls_return_req-number = '017'.
              APPEND ls_return_req TO et_return.

              CALL METHOD mo_st_pkr_util->read_request_with_reforder
                EXPORTING
                  is_pick_req = ls_pp_req
                IMPORTING
                  es_pick_req = ls_uw_pp_req
                  ev_subrc    = lv_subrc .

              IF lv_subrc IS NOT INITIAL.
                ls_pp_orders-storepickuporder = ls_pp_req-referencestorepickuporder.
                ls_pp_orders-store = ls_pp_req-store.
                ls_pp_orders-storeorderoverallstatus = 'W'.

                APPEND ls_pp_orders TO lt_pp_orders.

                CALL METHOD mo_str_ord->update_store_order
                  EXPORTING
                    it_st_pick_order = lt_pp_orders
                  IMPORTING
                    et_return        = DATA(lt_return_update).

              ENDIF.
            ENDIF.
            IF ls_return_req-type = 'E'.
              ls_return_req-number = '016'.
              APPEND ls_return_req TO et_return.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        LOOP AT lt_return_read INTO DATA(ls_return_read).
          APPEND ls_return_read TO et_return.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD update_picking_item.

*    DATA          : lo_st_pick_hdlr         TYPE REF TO if_rfm_st_pick_req_hndlr.
    DATA          : lt_pick_req TYPE rfm_st_pick_req_t.
    DATA          : lt_req_item TYPE rfm_st_pick_reqi_t.

    IF it_pp_req_item IS NOT INITIAL.

* fetch Picking request items

      CALL METHOD mo_st_pkr_util->read_items_with_keys
        EXPORTING
          it_pp_items   = it_pp_req_item
        IMPORTING
          et_pp_items = lt_req_item.

      IF sy-subrc EQ 0.
* Sfetch Picking requests associated with the items

        CALL METHOD mo_st_pkr_util->read_st_pick_req
          EXPORTING
            it_pp_item    = lt_req_item
          IMPORTING
            et_pp_request = lt_pick_req.

        IF lt_pick_req IS NOT INITIAL.
* Index read read is done to set the reference document type attribute in the handler class.

*          READ TABLE lt_pick_req INTO DATA(ls_pick_req) INDEX 1.
          cl_rfm_st_pick_request_factory=>mv_ref_doc_type = lt_pick_req[ 1 ]-storepickupordertype.
*          cl_rfm_st_pick_request_factory=>mv_ref_doc_type = ls_pick_req-storepickupordertype.
          mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
          IF mo_st_pick_hdlr IS BOUND.

            CALL METHOD mo_st_pick_hdlr->process_items
              EXPORTING
                it_pick_req   = lt_pick_req
                it_pick_items = it_pp_req_item
              IMPORTING
                et_return     = et_return
              CHANGING
                ct_pick_items = lt_req_item.

          ENDIF.
        ENDIF.

      ENDIF.
* Update item table if no error messages is found.

      READ TABLE et_return WITH KEY type = if_abap_behv_message=>severity-error TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        mo_st_pkr_util->update_st_pick_req_item(
        EXPORTING
          it_pp_req_item = lt_req_item ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD transfer_pick_request.

    DATA: lt_pick_req TYPE rfm_st_pick_req_t,
          ls_return   TYPE if_rfm_st_pick_request=>ty_behv_return.

    IF it_transfer_req IS NOT INITIAL.

      CALL METHOD mo_st_pkr_util->read_st_pick_req
        EXPORTING
          it_pp_req     = it_transfer_req
        IMPORTING
          et_pp_request = lt_pick_req.

      IF lt_pick_req IS NOT INITIAL.

        READ TABLE lt_pick_req INTO DATA(ls_pick_req) INDEX 1.
        cl_rfm_st_pick_request_factory=>mv_ref_doc_type = ls_pick_req-storepickupordertype.
        mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
        IF mo_st_pick_hdlr IS BOUND.

          CALL METHOD mo_st_pick_hdlr->transfer_requests
            EXPORTING
              it_pick_req     = lt_pick_req
              it_transfer_req = it_transfer_req
            IMPORTING
              et_return       = et_return.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD takeover_pick_request.

    DATA: lt_pick_req TYPE rfm_st_pick_req_t,
          ls_return   TYPE if_rfm_st_pick_request=>ty_behv_return.

    IF it_takeover_req IS NOT INITIAL.

      CALL METHOD mo_st_pkr_util->read_st_pick_req
        EXPORTING
          it_pp_req     = it_takeover_req
        IMPORTING
          et_pp_request = lt_pick_req.

      IF lt_pick_req IS NOT INITIAL.

        READ TABLE lt_pick_req INTO DATA(ls_pick_req) INDEX 1.
        cl_rfm_st_pick_request_factory=>mv_ref_doc_type = ls_pick_req-storepickupordertype.
        mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
        IF mo_st_pick_hdlr IS BOUND.

          CALL METHOD mo_st_pick_hdlr->takeover_requests
            EXPORTING
              it_req    = lt_pick_req
            IMPORTING
              et_return = et_return.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_objects.
    if_rfm_st_pick_request~set_object(
    io_str_ord = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( )
    io_st_pkr_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( )
    io_st_pkr_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( )
).
  ENDMETHOD.


  METHOD RELEASE_PICK_REQUEST.

    DATA: lt_pick_req TYPE rfm_st_pick_req_t,
          ls_return   TYPE if_rfm_st_pick_request=>ty_behv_return.

    IF it_release_req IS NOT INITIAL.

      CALL METHOD mo_st_pkr_util->read_st_pick_req
        EXPORTING
          it_pp_req     = it_release_req
        IMPORTING
          et_pp_request = lt_pick_req.

      IF lt_pick_req IS NOT INITIAL.

*        LOOP AT lt_pick_req ASSIGNING FIELD-SYMBOL(<ls_pick_req>).
*
*         CLEAR <ls_pick_req>-userassignedtopickingrequest.
*         <ls_pick_req>-storepickingrequeststatus = mv_rqst_notpicked.
*
*          ls_return-storepickingrequestid = <ls_pick_req>-storepickingrequest.
*          ls_return-store                 = <ls_pick_req>-store.
*          ls_return-id                    = mv_msg_id.
*          ls_return-type                  = if_abap_behv_message=>severity-success.
*          ls_return-number                = '026'.
*          ls_return-msgv1                 = <ls_pick_req>-storepickingrequest.
*          SHIFT ls_return-msgv1 LEFT DELETING LEADING '0'.
*          IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'S' NUMBER '026'. ENDIF.
*          APPEND ls_return TO et_return.
*
*        ENDLOOP.

        READ TABLE lt_pick_req INTO DATA(ls_pick_req) INDEX 1.
        cl_rfm_st_pick_request_factory=>mv_ref_doc_type = ls_pick_req-storepickupordertype.
        mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
        IF mo_st_pick_hdlr IS BOUND.
          CALL METHOD mo_st_pick_hdlr->release_requests
            EXPORTING
              it_req    = lt_pick_req
            IMPORTING
              et_return = et_return .
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD process_gtin.

    DATA: lo_rfm_xtract_gtin TYPE REF TO rfm_st_pick_req_xtrct_prdgtin.
    DATA: ls_product_gtin    TYPE if_rfm_st_pickreq_xtrct_prdgtn=>ty_gtin_extract.

    IF iv_scanned_gtin(2) BETWEEN '21' AND '29'.

      TRY .
          GET BADI lo_rfm_xtract_gtin. "BADI to call the custom gtin extract logicc
          IF lo_rfm_xtract_gtin IS BOUND.

            CALL BADI lo_rfm_xtract_gtin->extract_product_gtin
              EXPORTING
                scanned_gtin = iv_scanned_gtin                 " Scanned GTIN
              CHANGING
                product_gtin = ls_product_gtin.             " Extracted Product GTIN

            ev_weight = ls_product_gtin-weight.
            ev_weight_unit = ls_product_gtin-weightunit.
            ev_price = ls_product_gtin-price.
            ev_currency_unit = ls_product_gtin-currencyunit.
            ev_gtin = ls_product_gtin-gtin.

*            SELECT meinh FROM mean INTO ev_weight_unit WHERE ean11 = ls_product_gtin-gtin. ENDSELECT.

          ELSE.
            " in case the custom logic is not implemented
            ev_gtin = iv_scanned_gtin.
          ENDIF.
        CATCH cx_ble_runtime_error. " BLE: Runtime Error .

      ENDTRY.

    ELSE.
      ev_gtin = iv_scanned_gtin.
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~withdraw_pick_req.
    set_objects( ).

    CALL METHOD me->withdraw_picking
      EXPORTING
        it_pp_req = it_pp_req
      IMPORTING
        et_return = et_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~update_picking_item.

*  DATA: lv_msgty TYPE symsgty.
*    DATA: ls_return TYPE if_rtst_pp_request=>ty_behv_return.
*    AUTHORITY-CHECK OBJECT 'W_ST_PKRQ'
*    ID 'ACTVT' FIELD '02'.

*    IF sy-subrc EQ 0.
    set_objects( ).

    CALL METHOD me->update_picking_item
      EXPORTING
        it_pp_req_item = it_pp_req_item
      IMPORTING
        et_return      = et_return.

*    ELSE.
*      ls_return-id     = mv_msg_id.
*      ls_return-number = 031.
*      ls_return-type   = if_abap_behv_message=>severity-error.
*      IF 1 = 0.
*        MESSAGE ID 'RFM_RTST_PP' TYPE 'E' NUMBER '031'.
*      ENDIF.
*      APPEND ls_return TO et_return.
*    ENDIF.
*



*        IF cl_rfm_st_pick_order=>gv_call_update_task = abap_true.


*    LOOP AT et_return ASSIGNING FIELD-SYMBOL(<lfs_message>).
*
*      IF  <lfs_message>-type = if_abap_behv_message=>severity-error.
*        lv_msgty = 'E'.
*      ENDIF.
*      IF  <lfs_message>-type = if_abap_behv_message=>severity-success.
*        lv_msgty = 'S'.
*      ENDIF.
*
*      go_application_log->add_message(
*         EXPORTING
*           iv_msgno = <lfs_message>-number
*           iv_msgty =  lv_msgty
*           iv_msgid = <lfs_message>-id
*           iv_msgv1 = <lfs_message>-msgv1
*           iv_msgv2 = <lfs_message>-msgv2
*           iv_msgv3 = <lfs_message>-msgv3
*           iv_msgv4 = <lfs_message>-msgv4
*      ).
*    ENDLOOP.
*
*
*    go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_picking_req_api=>gc_appl_log_subobject ).
*    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~transfer_pick_request.

    set_objects( ).

    CALL METHOD me->transfer_pick_request
      EXPORTING
        it_transfer_req = it_transfer_req
      IMPORTING
        et_return       = et_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~takeover_pick_request.

    set_objects( ).

    CALL METHOD me->takeover_pick_request
      EXPORTING
        it_takeover_req = it_takeover_req
      IMPORTING
        et_return       = et_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~set_object.
    mo_str_ord = io_str_ord.
    mo_st_pkr_util = io_st_pkr_util.
    mo_st_pick_hdlr = io_st_pkr_hdlr.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request~release_pick_request.

    set_objects( ).

    CALL METHOD me->release_pick_request
      EXPORTING
        it_release_req = it_release_req
      IMPORTING
        et_return      = et_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~read_st_pick_request.
    set_objects( ).
  ENDMETHOD.


  METHOD if_rfm_st_pick_request~read_st_pick_bulk_request.
    set_objects( ).
  ENDMETHOD.


  METHOD if_rfm_st_pick_request~process_gtin.

    set_objects( ).

    CALL METHOD me->process_gtin
      EXPORTING
        iv_scanned_gtin  = iv_scanned_gtin
      IMPORTING
        ev_weight        = ev_weight
        ev_weight_unit   = ev_weight_unit
        ev_price         = ev_price
        ev_currency_unit = ev_currency_unit
        ev_gtin          = ev_gtin.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~finish_picking.
    CALL METHOD me->finish_picking.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request~delete_st_pick_request.

    set_objects( ).
    delete_st_pick_request(
      EXPORTING
        it_pp_req   =   it_pp_req               " Table type of structure RTST_PP_REQUEST
        iv_st_order =   iv_st_order             " Store pickup order
        it_pp_orders =  it_pp_orders
      IMPORTING
        et_return   =   et_return               " Table Type for BAPIRET2
    ).

        IF cl_rfm_st_pick_order=>gv_call_update_task = abap_true.
      LOOP AT et_return ASSIGNING FIELD-SYMBOL(<lfs_message>).
        go_application_log->add_message(
           EXPORTING
             iv_msgno = <lfs_message>-number
             iv_msgty = <lfs_message>-type
             iv_msgid = <lfs_message>-id
             iv_msgv1 = <lfs_message>-message_v1
             iv_msgv2 = <lfs_message>-message_v2
             iv_msgv3 = <lfs_message>-message_v3
             iv_msgv4 = <lfs_message>-message_v4 ).
      ENDLOOP.

      """ go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_picking_req_api=>gc_appl_log_subobject ).
      go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_picking_req_api=>gc_appl_log_subobject ).
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request~create_st_pick_request.
    set_objects( ).
    DATA: lv_ref_doc_type TYPE rfm_st_ref_doc_type.
    lv_ref_doc_type = '01'.
    READ TABLE it_st_pick_head INTO DATA(ls_st_pick_head) INDEX 1.
    mo_st_pkr_util->read_c_st_pickreq(
      EXPORTING
        iv_store        = ls_st_pick_head-store                 " Retail plant
        iv_ref_doc_type = lv_ref_doc_type                 " Document type of the reference document
      IMPORTING
        es_c_st_pickreq =  DATA(ls_c_st_pickreq)                " Customizing for generation of Store Picking Request
        et_return       =  DATA(lt_return_cust)                " Table Type for BAPIRET2
    ).
    IF lt_return_cust IS NOT INITIAL.
      APPEND LINES OF lt_return_cust TO et_return.
      RETURN.
    ENDIF.
    IF ls_c_st_pickreq IS NOT INITIAL .
      ms_c_st_pickreq = ls_c_st_pickreq.
    ENDIF.
    create_st_pick_request(
      EXPORTING
        it_st_pick_head =  it_st_pick_head                " Table type of structure RFM_ST_PICK_REQ
        it_st_pick_item =  it_st_pick_item                " Table type of structure RFM_ST_PICK_REQI
      IMPORTING
        et_return       =  DATA(lt_return_create)                " Table Type for BAPIRET2
        et_st_pick_head = et_st_pick_head                 " Table type of structure RFM_ST_PICK_REQ
        et_st_pick_item = et_st_pick_item                 " Table type of structure RFM_ST_PICK_REQI
    ).
    IF lt_return_create IS NOT INITIAL .
      APPEND LINES OF lt_return_create TO et_return.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_request~create_picking_item.

    set_objects( ).

    CALL METHOD me->create_picking_item
      EXPORTING
        it_pp_req_item = it_pp_req_item
      IMPORTING
        et_return      = et_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~begin_picking.

    set_objects( ).
    DATA: lt_return TYPE if_rfm_st_pick_request=>tt_behv_return.
    CALL METHOD me->begin_picking
      EXPORTING
        it_rtst_pkr_print = it_rtst_pkr_print
      IMPORTING
        et_return         = et_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_request~assign_storage.
    set_objects( ).
    CALL METHOD me->assign_storage
      EXPORTING
        it_pp_req = it_pp_req
      IMPORTING
        et_return = et_return.
  ENDMETHOD.


  METHOD finish_picking.
    DATA lt_messages TYPE tt_bapiret2.

    gv_action_from_api = abap_true.

    CALL FUNCTION 'RFM_ST_PICK_REQ_FINSH_RFC' IN BACKGROUND TASK
      EXPORTING
        iv_storepickingrequest = if_rfm_st_pick_request~gv_picking_request
      TABLES
        it_sales_ord_details   = if_rfm_st_pick_request~gt_fnsh_pick_itm
        et_messages            = lt_messages.

  ENDMETHOD.


  METHOD delete_st_pick_request.

    DATA: lt_return_del_ord TYPE tt_bapiret2,
          lt_return_del_req TYPE tt_bapiret2.

    IF iv_st_order IS NOT INITIAL AND it_pp_req IS INITIAL.

      " if deletion is requested at order level
      mo_st_pkr_util->read_st_pick_req(
        EXPORTING
          iv_st_order   =  iv_st_order     " Store pickup order
        IMPORTING
          et_return     = DATA(lt_return_read_ord) " Table Type for BAPIRET2
          et_pp_request = DATA(lt_pp_req_ord)              " Table type of structure RTST_PP_REQUEST
      ).
      " now check if we have no errors in reading the picking requests
      IF lt_return_read_ord IS INITIAL.

        mo_st_pkr_util->delete_st_pick_req(
        EXPORTING
           it_pp_req = lt_pp_req_ord                 " Table type of structure RTST_PP_REQUEST
        IMPORTING
           et_return = lt_return_del_ord       " Table Type for BAPIRET2
        ).
      ELSE.
        APPEND LINES OF lt_return_read_ord TO et_return.
      ENDIF.

*---------------------------------------------------------------------------------
    ELSEIF it_pp_req IS NOT INITIAL AND iv_st_order IS INITIAL.

      " if deletion is requested at picking request level
      mo_st_pkr_util->read_st_pick_req(
        EXPORTING
          it_pp_req     = it_pp_req                 " Table type of structure RTST_PP_REQUEST
        IMPORTING
          et_return     = DATA(lt_return_read_req)                 " Table Type for BAPIRET2
          et_pp_request = DATA(lt_pp_req_r)                 " Table type of structure RTST_PP_REQUEST
      ).
      " now check if we have no errors in reading the picking requests
      IF lt_return_read_req IS INITIAL.

        mo_st_pkr_util->delete_st_pick_req(
      EXPORTING
        it_pp_req = lt_pp_req_r   " Table type of structure RTST_PP_REQUEST
      IMPORTING
        et_return = lt_return_del_req              " Table Type for BAPIRET2
    ).

      ELSE.
        APPEND LINES OF lt_return_read_req TO et_return.
      ENDIF.
*---------------------------------------------------------------------------------
    ELSEIF it_pp_orders IS NOT INITIAL.

      " if deletion is requested with more than one orders
*           first read all the picking requests for the table of orders
      mo_st_pkr_util->read_st_pick_req(
        EXPORTING
          it_pp_orders  = it_pp_orders                 " Table type of structure RTST_PP_ORDERS
        IMPORTING
          et_return     = DATA(lt_return_read_ord_t)               " Table Type for BAPIRET2
          et_pp_request = DATA(lt_pp_req_ord_t)                 " Table type of structure RTST_PP_REQUEST
      ).

      " if no errors in reading picking requests
      IF lt_return_read_ord_t IS INITIAL.

        mo_st_pkr_util->delete_st_pick_req(
          EXPORTING
            it_pp_req =  lt_pp_req_ord_t  " Table type of structure RTST_PP_REQUEST
          IMPORTING
            et_return =  DATA(lt_return_del_ord_t)                " Table Type for BAPIRET2
        ).

      ELSE.
        APPEND LINES OF lt_return_read_ord_t TO et_return.
      ENDIF.

    ENDIF.
*    append all the error messages to et_return at the end
    IF  lt_return_del_ord IS NOT INITIAL .
      APPEND LINES OF lt_return_del_ord TO et_return.
    ENDIF.

    IF  lt_return_del_req IS NOT INITIAL .
      APPEND LINES OF lt_return_del_req TO et_return.
    ENDIF.

    IF  lt_return_del_ord_t IS NOT INITIAL .
      APPEND LINES OF lt_return_del_ord_t TO et_return.
    ENDIF.
  ENDMETHOD.


  METHOD create_st_pick_request.

    IF it_st_pick_head IS NOT INITIAL AND it_st_pick_item IS NOT INITIAL.
      DATA: lv_temp_id      TYPE rfm_st_pick_request,
            lv_nr_range     TYPE rfm_c_st_pickreq-store_picking_nr_seq,
            lv_pkr_id       TYPE rfm_st_pick_request,
            ls_return       TYPE bapiret2,
            lt_st_pick_head TYPE rfm_st_pick_req_t,
            lt_st_pick_item TYPE rfm_st_pick_reqi_t.
       lt_st_pick_head = it_st_pick_head.
       lt_st_pick_item = it_st_pick_item.
      LOOP AT lt_st_pick_head ASSIGNING FIELD-SYMBOL(<fs_pkr_head>).
        IF ms_c_st_pickreq-store_picking_nr_seq IS NOT INITIAL.
          lv_nr_range = ms_c_st_pickreq-store_picking_nr_seq.
        ELSE.
*        raise error message here and roll back work
          ls_return-id = 'RFM_ST_PICK_REQ'.
          ls_return-number = '032'.
          ls_return-type = 'E'.
          IF 1 = 2.MESSAGE e032(rfm_st_pick_req).ENDIF.
          APPEND ls_return TO et_return.
          RETURN.
        ENDIF.
        CALL METHOD mo_st_pkr_util->get_picking_req_number
          EXPORTING
            iv_number = lv_nr_range
            iv_object = 'RTST_PP'
          IMPORTING
            ev_number = lv_pkr_id
            et_return = DATA(lt_return_nr).
        IF lt_return_nr IS NOT INITIAL.
          APPEND LINES OF lt_return_nr TO et_return.
          RETURN.
        ENDIF.

        LOOP AT lt_st_pick_item ASSIGNING FIELD-SYMBOL(<fs_pkr_item>) WHERE storepickingrequest = <fs_pkr_head>-storepickingrequest.
          <fs_pkr_item>-storepickingrequest = lv_pkr_id.
        ENDLOOP.
        <fs_pkr_head>-storepickingrequest = lv_pkr_id.
      ENDLOOP.
      APPEND LINES OF lt_st_pick_head TO et_st_pick_head.
      APPEND LINES OF lt_st_pick_item TO et_st_pick_item.
    ENDIF.
  ENDMETHOD.


METHOD create_picking_item.

  DATA: lt_pick_req TYPE rfm_st_pick_req_t.

  IF it_pp_req_item IS NOT INITIAL.

* fetch Picking requests associated with the items

    CALL METHOD mo_st_pkr_util->read_st_pick_req
      EXPORTING
        it_pp_item    = it_pp_req_item
      IMPORTING
        et_pp_request = lt_pick_req.

    IF lt_pick_req IS NOT INITIAL.
* Index read read is done to set the reference document type attribute in the handler class.

      cl_rfm_st_pick_request_factory=>mv_ref_doc_type = lt_pick_req[ 1 ]-storepickupordertype.
      mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
      IF mo_st_pick_hdlr IS BOUND.

        CALL METHOD mo_st_pick_hdlr->create_items
          EXPORTING
            it_pick_req   = lt_pick_req
            it_pick_items = it_pp_req_item
          IMPORTING
            et_return     = et_return.

      ENDIF.

    ENDIF.

  ENDIF.

ENDMETHOD.


  METHOD create.
    ro_st_pkr = NEW cl_rfm_st_pick_request(  ) .
  ENDMETHOD.


  method CONSTRUCTOR.
     go_application_log = cl_rtst_application_log=>get_instance( ).
  endmethod.


  METHOD begin_picking.

    DATA:
      lt_rtst_pp_req_item TYPE rfm_st_pick_reqi_t,
      ls_return           TYPE if_rfm_st_pick_request=>ty_behv_return,
      lt_pick_req_head    TYPE rfm_st_pick_req_t,
*      lo_st_ord_util      TYPE REF TO if_rfm_st_pick_order_utility,
      lt_store_order_pmnt TYPE rfm_st_pick_ord_t,
      lv_tabix            TYPE sy-tabix,
      lv_sequence         TYPE i VALUE 0,
      lt_return           TYPE if_rfm_st_pick_request=>tt_behv_return.

    IF it_rtst_pkr_print IS NOT INITIAL. " first we just check if the input table is filled, if not we do not do any further processing.

      MOVE-CORRESPONDING it_rtst_pkr_print TO lt_pick_req_head.    "Move corresponding here ignores the incomming parameters because of different datatypes.

      CALL METHOD mo_st_pkr_util->read_st_pick_req
        EXPORTING
          it_pp_req     = lt_pick_req_head
        IMPORTING
          et_return     = DATA(lt_return_req)
          et_pp_request = DATA(lt_picking_req).
      lt_store_order_pmnt = CORRESPONDING #( lt_picking_req MAPPING storepickuporder = referencestorepickuporder ).
*      lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

*   **  READ OF PICK ORDERS HERE
      DATA(lt_pick_req) = lt_picking_req.

      LOOP AT lt_picking_req ASSIGNING FIELD-SYMBOL(<ls_picking_req>).
        IF <ls_picking_req>-storepickingrequeststatus EQ mv_rqst_notpicked.  " check if the picking request is not yet assigned to any user

          <ls_picking_req>-storepickingrequeststatus    = mv_rqst_inprocess. " change the status to A i.e. in process
          <ls_picking_req>-userassignedtopickingrequest = sy-uname. " assign the current user
          ls_return-storepickingrequestid = <ls_picking_req>-storepickingrequest.
          ls_return-store                 = <ls_picking_req>-store.
          ls_return-id                    = mv_msg_id.
          ls_return-type                  = if_abap_behv_message=>severity-success.
          ls_return-number                = '026'.
            ls_return-msgv1                 = <ls_picking_req>-storepickingrequest.
            SHIFT ls_return-msgv1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'S' NUMBER '026'. ENDIF.
          APPEND ls_return TO et_return.

        ELSEIF <ls_picking_req>-userassignedtopickingrequest NE sy-uname.   " if already picking request is assigned to a user and someone else is trying to process it, raise a message

          READ TABLE it_rtst_pkr_print INTO DATA(ls_rtst_pkr_print3) WITH KEY storepickingrequest = <ls_picking_req>-storepickingrequest.
          IF sy-subrc EQ 0.
            ls_return-storepickingrequestid = <ls_picking_req>-storepickingrequest.
            ls_return-store                 = <ls_picking_req>-store.
            ls_return-id                    = mv_msg_id.
            ls_return-type                  = if_abap_behv_message=>severity-error.
            ls_return-number                = '013'.
            ls_return-msgv1                 = <ls_picking_req>-storepickingrequest.
            SHIFT ls_return-msgv1 LEFT DELETING LEADING '0'.
            ls_return-msgv2                 = <ls_picking_req>-userassignedtopickingrequest.
            IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'E' NUMBER '013'. ENDIF.
            APPEND ls_return TO et_return.
          ENDIF.
        ELSEIF <ls_picking_req>-userassignedtopickingrequest EQ sy-uname.

          ls_return-storepickingrequestid = <ls_picking_req>-storepickingrequest.
          ls_return-store                 = <ls_picking_req>-store.
          ls_return-id                    = mv_msg_id.
          ls_return-type                  = if_abap_behv_message=>severity-success.
          ls_return-number                = '026'.
            ls_return-msgv1                 = <ls_picking_req>-storepickingrequest.
            SHIFT ls_return-msgv1 LEFT DELETING LEADING '0'.
          IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'S' NUMBER '026'. ENDIF.
          APPEND ls_return TO et_return.
        ENDIF.

      ENDLOOP.

      READ TABLE et_return WITH KEY type = if_abap_behv_message=>severity-error TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.
*      decouple the call for reference document type
      READ TABLE lt_picking_req INTO DATA(ls_picking_req) INDEX 1.
      cl_rfm_st_pick_request_factory=>mv_ref_doc_type = ls_picking_req-storepickupordertype.
*      DATA: lo_st_pick_hdlr TYPE REF TO if_rfm_st_pick_req_hndlr.

      mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
      IF mo_st_pick_hdlr IS BOUND.
        CALL METHOD mo_st_pick_hdlr->print
          EXPORTING
            it_pick_req  = lt_pick_req
            it_print_tab = it_rtst_pkr_print
          CHANGING
            et_return    = lt_return.

        READ TABLE lt_return WITH KEY type = if_abap_behv_message=>severity-error TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          et_return = lt_return.
          RETURN.
        ENDIF.
      ENDIF.

      IF lt_picking_req IS NOT INITIAL.

        mo_st_pkr_util->update_st_pick_req(
          EXPORTING
            it_pp_req = lt_picking_req                 " Table type of structure RTST_PP_REQUEST
            IMPORTING
              et_return = DATA(lt_return_update_req)     " Table Type for BAPIRET2
        ).
*        IF lt_return_update_req IS NOT INITIAL.
*          APPEND LINES OF lt_return_update_req TO et_return.
*        ENDIF.
*        as we have updated the picking request status to begin picking, we need to update the store order table rtst_pp_orders as well with the same status
        IF mo_st_pick_hdlr IS BOUND.
          CALL METHOD mo_st_pick_hdlr->update_ref_order
            EXPORTING
              it_pick_req = lt_picking_req
            IMPORTING
              et_return   = DATA(lt_return_update).
          IF lt_return_update IS NOT INITIAL.
            et_return = lt_return_update.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD assign_storage.
    DATA: lv_msgty TYPE symsgty.
    DATA: ls_return        TYPE if_rfm_st_pick_request=>ty_behv_return,
          lt_order         TYPE rfm_st_pick_ord_t,
          ls_order         TYPE rfm_st_pick_ord,
          ls_return_update TYPE bapiret2,
          lt_return_update TYPE tt_bapiret2,
          ls_fnsh_pick_itm TYPE rtst_st_sales_ord_det,
          lv_tolerance_up  TYPE rfm_st_pick_qty,
          lv_tolerance_low TYPE rfm_st_pick_qty,
          lv_limit         TYPE rfm_st_pick_qty,
          lo_st_pick_hdlr  TYPE REF TO if_rfm_st_pick_req_hndlr.

    IF it_pp_req IS NOT INITIAL.

      CALL METHOD mo_st_pkr_util->read_st_pick_req
        EXPORTING
          it_pp_req     = it_pp_req
        IMPORTING
          et_return     = DATA(lt_return_read)
          et_pp_request = DATA(lt_pp_req_head).
      IF lt_return_read IS NOT INITIAL.
        et_return = lt_return_read.
        RETURN.
      ENDIF.

      IF lt_pp_req_head IS NOT INITIAL.
        READ TABLE lt_pp_req_head INTO DATA(ls_pp_req_head) INDEX 1.
        cl_rfm_st_pick_request_factory=>mv_ref_doc_type = ls_pp_req_head-storepickupordertype.
        mo_st_pick_hdlr = cl_rfm_st_pick_request_factory=>get( )->get_st_pick_req_hndlr( ).
        IF mo_st_pick_hdlr IS BOUND.

          CALL METHOD mo_st_pick_hdlr->process_requests
            EXPORTING
              it_pick_req = it_pp_req
            IMPORTING
              et_return   = et_return
            CHANGING
              ct_pick_req = lt_pp_req_head.

        ENDIF.

        CALL METHOD mo_st_pkr_util->update_st_pick_req
          EXPORTING
            it_pp_req = lt_pp_req_head
          IMPORTING
            et_return = lt_return_update.

        IF lt_return_update IS NOT INITIAL.
          READ TABLE lt_return_update INTO ls_return_update INDEX 1.
          IF ls_return_update-type = 'S'.
            ls_return_update-number = '001'.

            CALL METHOD mo_st_pick_hdlr->update_ref_order
              EXPORTING
                it_pick_req = lt_pp_req_head
              IMPORTING
                et_return   = DATA(lt_return_ord_upd).

          ELSEIF ls_return_update-type = 'E'.
            ls_return-id = 'RFM_ST_PICK_REQ'.
            ls_return-type = if_abap_behv_message=>severity-error.
            ls_return-number = '034'.
            APPEND ls_return TO et_return.
            IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'E' NUMBER '034'. ENDIF.
          ENDIF.

        ENDIF.
      ELSE.
        ls_return-id = 'RFM_ST_PICK_REQ'.
        ls_return-type = if_abap_behv_message=>severity-error.
        ls_return-number = '035'.
        APPEND ls_return TO et_return.
        IF 1 = 0. MESSAGE ID 'RFM_ST_PICK_REQ' TYPE 'E' NUMBER '035'. ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.