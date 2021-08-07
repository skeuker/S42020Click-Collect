CLASS lhc_i_storepickupordertp DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA:lo_st_ord        TYPE REF TO if_rfm_st_pick_order,
         lo_pick_req_util TYPE REF TO if_rfm_st_pick_request_utility,
         lo_st_ord_util   TYPE REF TO if_rfm_st_pick_order_utility.

    DATA: lt_store_order TYPE rfm_st_pick_ord_t , "rtst_pp_orders_t,
          is_store_order TYPE rfm_st_pick_ord,
          lt_return      TYPE tt_bapiret2.

    CONSTANTS : lc_message_class           TYPE symsgid VALUE  'RFM_ST_PICK_ORD',
                lc_handover_picking_status TYPE rfm_st_order_picking_status VALUE  '05',
                lc_handover_overall_status TYPE rfm_st_order_overall_status VALUE  'H'.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE i_storepickupordertp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE i_storepickupordertp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE i_storepickupordertp.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK i_storepickupordertp.

    METHODS read FOR READ
      IMPORTING keys FOR READ i_storepickupordertp RESULT result.

    METHODS processorderhandover FOR MODIFY IMPORTING keys   FOR ACTION i_storepickupordertp~processorderhandover
                                            RESULT    result.

    METHODS processresvnorderhndovr FOR MODIFY IMPORTING it_process_reservationhandover
                                                           FOR ACTION i_storepickupordertp~processresvnorderhndovr
                                               RESULT    result.

    METHODS processstoreordercancellation FOR MODIFY IMPORTING it_process_ordercancellation
                                                                 FOR ACTION i_storepickupordertp~processstoreordercancellation
                                                     RESULT    result.

    METHODS cba_storepickupordexcptnitem FOR MODIFY
      IMPORTING entities_cba FOR CREATE i_storepickupordertp\_storepickupordexcptnitem.

    METHODS rba_storepickupordexcptnitem FOR READ
      IMPORTING keys_rba FOR READ i_storepickupordertp\_storepickupordexcptnitem FULL result_requested RESULT result LINK association_links.

    METHODS cba_storepickupordpickeditem FOR MODIFY
      IMPORTING entities_cba FOR CREATE i_storepickupordertp\_storepickupordpickeditem.

    METHODS rba_storepickupordpickeditem FOR READ
      IMPORTING keys_rba FOR READ i_storepickupordertp\_storepickupordpickeditem FULL result_requested RESULT result LINK association_links.

    METHODS cba_storepickupordpickingreq FOR MODIFY
      IMPORTING entities_cba FOR CREATE i_storepickupordertp\_storepickupordpickingreq.

    METHODS rba_storepickupordpickingreq FOR READ
      IMPORTING keys_rba FOR READ i_storepickupordertp\_storepickupordpickingreq FULL result_requested RESULT result LINK association_links.

    METHODS _check_auth_for_withdraw IMPORTING i_activity                 TYPE num2
                                               i_store                    TYPE werks_d
                                     RETURNING VALUE(allowed_to_withdraw) TYPE boole_d.

ENDCLASS.

CLASS lhc_i_storepickupordertp IMPLEMENTATION.

  METHOD create.
    DATA lt_pp_orders TYPE rfm_st_pick_ord_t.
    lo_st_ord_util      = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

    IF lo_st_ord_util IS BOUND.
      MOVE-CORRESPONDING entities TO lt_pp_orders.
* Create authorization has been checked within below method
      lo_st_ord_util->insert_st_pick_orders(
        EXPORTING
          it_pp_orders   = lt_pp_orders                 " Table type for structure RTST_PP_ORDERS
        IMPORTING
          et_return      =  DATA(lt_return)             " Table Type for BAPIRET2
      ).

      IF lt_return IS NOT INITIAL.
*        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        LOOP AT lt_return INTO DATA(ls_return).
          CASE ls_return-type.
            WHEN 'S'.
              APPEND VALUE #( %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                  v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                   severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

            WHEN 'I'.
              APPEND VALUE #(  %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                     severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

            WHEN 'E'.
              APPEND VALUE #(  %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                     severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.
          ENDCASE.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD delete.

    DATA lt_pick_order_delete TYPE rfm_st_pick_ord_t.
    DATA : lo_st_pick_req TYPE REF TO if_rfm_st_pick_request.
    lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).
    IF lo_st_ord_util IS BOUND.
      MOVE-CORRESPONDING  keys TO lt_pick_order_delete.
* Authorization check has been done within below method
      lo_st_ord_util->delete_st_pick_order(
        EXPORTING
          it_st_order =  lt_pick_order_delete               " Store pickup order
        IMPORTING
          et_return   = DATA(lt_return_tab)                 " Table Type for BAPIRET2
      ).

      IF lt_return_tab IS NOT INITIAL.
*        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        LOOP AT lt_return_tab INTO DATA(ls_return).
          CASE ls_return-type.
            WHEN 'S'.
              APPEND VALUE #( %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                  v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                   severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

            WHEN 'I'.
              APPEND VALUE #(  %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                     severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

            WHEN 'E'.
              APPEND VALUE #(  %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                     severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.
          ENDCASE.
        ENDLOOP.
      ELSE.
        IF lo_st_pick_req IS NOT BOUND.
          lo_st_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
        ENDIF.
        lo_st_pick_req->delete_st_pick_request(
          EXPORTING
            it_pp_orders =  lt_pick_order_delete  " Table type for structure RFM_ST_PICK_ORD
          IMPORTING
            et_return    = DATA(lt_return_pkr_del)                 " Table Type for BAPIRET2
        ).
        IF lt_return_pkr_del IS NOT INITIAL.
          LOOP AT lt_return_pkr_del INTO DATA(ls_return_pkr_del).
            CASE ls_return_pkr_del-type.
              WHEN 'S'.
                APPEND VALUE #( %msg = new_message( id = ls_return_pkr_del-id number = ls_return_pkr_del-number v1 = ls_return_pkr_del-message_v1
                                                    v2 = ls_return_pkr_del-message_v2  v3 = ls_return_pkr_del-message_v3
                                                     severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

              WHEN 'I'.
                APPEND VALUE #(  %msg = new_message( id = ls_return_pkr_del-id number = ls_return_pkr_del-number v1 = ls_return_pkr_del-message_v1
                                                     v2 = ls_return_pkr_del-message_v2  v3 = ls_return_pkr_del-message_v3
                                                       severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

              WHEN 'E'.
                APPEND VALUE #(  %msg = new_message( id = ls_return_pkr_del-id number = ls_return_pkr_del-number v1 = ls_return_pkr_del-message_v1
                                                     v2 = ls_return_pkr_del-message_v2  v3 = ls_return_pkr_del-message_v3
                                                       severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.
            ENDCASE.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD update.
* Get sales order org data
*  IF _check_auth_for_instance( i_activity = '02' i_auart = lv_auart i_spart = lv_spart
*                               i_vkorg = lv_vkorg i_vtweg = lv_vtweg ) = abap_true.
*  ENDIF.
    DATA lt_pick_order TYPE rfm_st_pick_ord_t.
    lo_st_ord = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( ).
    IF lo_st_ord IS BOUND.

      MOVE-CORRESPONDING entities TO lt_pick_order.
      lo_st_ord->update_store_order(
         EXPORTING it_st_pick_order = lt_pick_order
         IMPORTING et_return = DATA(lt_return)
                   et_st_pick_order = DATA(lt_st_pick_order) ).

      IF lt_return IS NOT INITIAL.
*        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        LOOP AT lt_return INTO DATA(ls_return).
          CASE ls_return-type.
            WHEN 'S'.
              APPEND VALUE #( %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                  v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                   severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

            WHEN 'I'.
              APPEND VALUE #(  %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                     severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

            WHEN 'E'.
              APPEND VALUE #(  %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   v2 = ls_return-message_v2  v3 = ls_return-message_v3
                                                     severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.
          ENDCASE.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD read.
* Get sales order org data
*  IF _check_auth_for_instance( i_activity = '03' i_auart = lv_auart i_spart = lv_spart
*                               i_vkorg = lv_vkorg i_vtweg = lv_vtweg ) = abap_true.
*  ENDIF.
  ENDMETHOD.

  METHOD lock.

*    READ TABLE keys into data(ls_lock_key) index 1.
*
*    SET LOCKS OF I_StoreHandoverOrderTP ENTITY I_StoreHandoverOrderTP
*    FROM VALUE #( FOR keyval IN keys ( Store = ls_lock_key-Store  StorePickUpOrder = ls_lock_key-StorePickUpOrder ) )
*     FAILED DATA(md_failed)
*     REPORTED DATA(md_reported).
*
*    failed-i_storehandoverordertp = VALUE #( FOR failed_order
*                                          IN md_failed-i_storehandoverordertp ( %cid = failed_order-%cid
*                                                                      %key = failed_order-%key
*                                                                      %fail = failed_order-%fail
*                                                                      %update = failed_order-%update
*                                                                      %action = failed_order-%action ) ).
*
*    reported-i_storehandoverordertp = VALUE #( FOR reported_order
*                                            IN md_reported-i_storehandoverordertp ( %cid = reported_order-%cid
*                                                                          %key = reported_order-%key
*                                                                          %msg = reported_order-%msg
*                                                                          %element = reported_order-%element
*                                                                          %state_area = reported_order-%state_area ) ).


  ENDMETHOD.

******************************
** Hand Over of store order **
******************************

  METHOD processorderhandover.
*    lo_st_ord = cl_rfm_st_ord_factory=>get( )->get_st_ord_obj( ).
    lo_st_ord      = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( ).
    lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

    ASSERT lines( keys ) = 1.
*    READ TABLE keys INTO DATA(ls_keys) INDEX 1.
    DATA(ls_keys) = keys[ 1 ].

    is_store_order-store                   = ls_keys-store.
    is_store_order-storepickuporder        = ls_keys-storepickuporder.
    is_store_order-storeorderpickingstatus = lc_handover_picking_status.
    is_store_order-storeorderoverallstatus = lc_handover_overall_status.
    APPEND is_store_order TO lt_store_order.

** Get organizational data
    CALL METHOD lo_st_ord_util->get_vbak_single
      EXPORTING
        iv_vbeln = ls_keys-storepickuporder
      IMPORTING
        es_vbak  = DATA(ls_salesorderheader).

** Authorization Check

    IF lo_st_ord_util->check_authorization( i_activity = '02' i_auart = ls_salesorderheader-auart i_spart = ls_salesorderheader-spart
                                            i_vkorg = ls_salesorderheader-vkorg i_vtweg = ls_salesorderheader-vtweg ) = abap_true
    AND _check_auth_for_withdraw( i_activity = 75  i_store = ls_keys-store  ) = abap_true.

** Update Pickup Order
      CALL METHOD lo_st_ord->update_store_order
        EXPORTING
          it_st_pick_order = lt_store_order
        IMPORTING
          et_return        = lt_return.

      IF lt_return IS NOT INITIAL.
        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        CASE ls_return-type.
          WHEN 'S'.
            lo_st_ord->gv_action     = 'ProcessOrderHandover'.
            lo_st_ord->gv_storeorder = ls_keys-storepickuporder.
            lo_st_ord->gv_store      = ls_keys-store.
            APPEND VALUE #( store = ls_keys-store  storepickuporder = ls_keys-storepickuporder
                            %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                  severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

          WHEN 'I'.
            APPEND VALUE #( store = ls_keys-store  storepickuporder = ls_keys-storepickuporder
                             %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

          WHEN 'E'.

            APPEND VALUE #( store = ls_keys-store  storepickuporder = ls_keys-storepickuporder
                             %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.

        ENDCASE.
      ENDIF.
    ELSE.
      APPEND VALUE #( store = ls_keys-store  storepickuporder = ls_keys-storepickuporder
                      %msg = new_message( id = lc_message_class number = '033' severity = if_abap_behv_message=>severity-error ) ) TO reported-i_storepickupordertp.
      IF 1 = 2. MESSAGE e033(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
    ENDIF.

*    ENDIF.

  ENDMETHOD.
******************************************
** Reservation hand over of store order **
******************************************
  METHOD processresvnorderhndovr.

*    lo_st_ord = cl_rfm_st_ord_factory=>get( )->get_st_ord_obj( ).
    lo_st_ord = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( ).
    lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

    ASSERT lines( it_process_reservationhandover ) = 1.
*   READ TABLE it_process_reservationhandover INTO DATA(ls_process_reservationhandover) INDEX 1.
    DATA(ls_process_reservationhandover) = it_process_reservationhandover[ 1 ].

*   IF sy-subrc = 0.
    is_store_order-store                   = ls_process_reservationhandover-%param-store.
    is_store_order-storepickuporder        = ls_process_reservationhandover-%param-storepickuporder.
    is_store_order-storeorderpickingstatus = ls_process_reservationhandover-%param-storeorderpickingstatus.
    is_store_order-storeorderoverallstatus = ls_process_reservationhandover-%param-storeorderoverallstatus.

    APPEND is_store_order TO lt_store_order.

** Get organizational data
    CALL METHOD lo_st_ord_util->get_vbak_single
      EXPORTING
        iv_vbeln = ls_process_reservationhandover-%param-storepickuporder
      IMPORTING
        es_vbak  = DATA(ls_salesorderheader).

** Authorization Check
    IF lo_st_ord_util->check_authorization( i_activity = '02' i_auart = ls_salesorderheader-auart i_spart = ls_salesorderheader-spart
                                            i_vkorg = ls_salesorderheader-vkorg i_vtweg = ls_salesorderheader-vtweg ) = abap_true
    AND _check_auth_for_withdraw( i_activity = 75  i_store = ls_process_reservationhandover-%param-store  ) = abap_true.

      CALL METHOD lo_st_ord->update_store_order
        EXPORTING
          it_st_pick_order = lt_store_order
        IMPORTING
          et_return        = lt_return.

      IF lt_return IS NOT INITIAL.
        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        CASE ls_return-type.
          WHEN 'S'.
            lo_st_ord->gv_action                   = 'ProcessResvnOrderHndovr'.
            lo_st_ord->gv_store                    = ls_process_reservationhandover-%param-store.
            lo_st_ord->gv_storeorder               = ls_process_reservationhandover-%param-storepickuporder.
            lo_st_ord->gv_rejection_reason_code_rh = ls_process_reservationhandover-%param-storeresvnhndovrreason.
            APPEND VALUE #( store = ls_process_reservationhandover-%param-store
                            storepickuporder = ls_process_reservationhandover-%param-storepickuporder
                            %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                  severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

          WHEN 'I'.
            APPEND VALUE #( store = ls_process_reservationhandover-%param-store
                            storepickuporder = ls_process_reservationhandover-%param-storepickuporder
                             %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

          WHEN 'E'.

            APPEND VALUE #( store = ls_process_reservationhandover-%param-store
                            storepickuporder = ls_process_reservationhandover-%param-storepickuporder
                             %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.

        ENDCASE.
      ENDIF.
    ELSE.

      APPEND VALUE #( store = ls_process_reservationhandover-%param-store
                      storepickuporder = ls_process_reservationhandover-%param-storepickuporder
                       %msg = new_message( id = lc_message_class number = '034' severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.

      IF 1 = 2. MESSAGE e034(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
    ENDIF.

*    ENDIF.
  ENDMETHOD.

**********************************
** Cancellation of store order  **
**********************************
  METHOD processstoreordercancellation.

    lo_st_ord = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( ).
    lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

    ASSERT lines( it_process_ordercancellation ) = 1.
*    READ TABLE it_process_ordercancellation INTO DATA(ls_process_ordercancellation) INDEX 1.
    DATA(ls_process_ordercancellation) = it_process_ordercancellation[ 1 ].

*    IF sy-subrc = 0.
    is_store_order-store                      = ls_process_ordercancellation-%param-store.
    is_store_order-storepickuporder           = ls_process_ordercancellation-%param-storepickuporder.
    is_store_order-storeorderpickingstatus    = ls_process_ordercancellation-%param-storeorderpickingstatus.
    is_store_order-storeorderoverallstatus    = ls_process_ordercancellation-%param-storeorderoverallstatus.
    is_store_order-storeorderrejectionreason  = ls_process_ordercancellation-%param-storeorderrejectionreason.

    APPEND is_store_order TO lt_store_order.

** Get organizational data
    CALL METHOD lo_st_ord_util->get_vbak_single
      EXPORTING
        iv_vbeln = ls_process_ordercancellation-%param-storepickuporder
      IMPORTING
        es_vbak  = DATA(ls_salesorderheader).

** Authorization Check
    IF lo_st_ord_util->check_authorization( i_activity = '02' i_auart = ls_salesorderheader-auart i_spart = ls_salesorderheader-spart
                                            i_vkorg = ls_salesorderheader-vkorg i_vtweg = ls_salesorderheader-vtweg ) = abap_true
    AND _check_auth_for_withdraw( i_activity = 75  i_store = ls_process_ordercancellation-%param-store  ) = abap_true.

      CALL METHOD lo_st_ord->update_store_order
        EXPORTING
          it_st_pick_order = lt_store_order
        IMPORTING
          et_return        = lt_return.


      IF lt_return IS NOT INITIAL.
        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        CASE ls_return-type.
          WHEN 'S'.
            lo_st_ord->gv_action                   = 'ProcessStoreOrderCancellation'.
            lo_st_ord->gv_store                    = ls_process_ordercancellation-%param-store.
            lo_st_ord->gv_storeorder               = ls_process_ordercancellation-%param-storepickuporder .
            lo_st_ord->gv_rejection_reason_code    = ls_process_ordercancellation-%param-storeorderrejectionreason.
            lo_st_ord->gv_rejection_comments       = ls_process_ordercancellation-%param-storeorderrejectioncomment.
            APPEND VALUE #( store = ls_process_ordercancellation-%param-store
                            storepickuporder = ls_process_ordercancellation-%param-storepickuporder
                              %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                    severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordertp.

          WHEN 'I'.
            APPEND VALUE #( store = ls_process_ordercancellation-%param-store
                            storepickuporder = ls_process_ordercancellation-%param-storepickuporder
                             %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickupordertp.

          WHEN 'E'.

            APPEND VALUE #( store = ls_process_ordercancellation-%param-store
                            storepickuporder = ls_process_ordercancellation-%param-storepickuporder
                             %msg = new_message( id = ls_return-id number = ls_return-number v1 = ls_return-message_v1
                                                   severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.

        ENDCASE.
      ENDIF.
    ELSE.
      APPEND VALUE #( store = ls_process_ordercancellation-%param-store
                      storepickuporder = ls_process_ordercancellation-%param-storepickuporder
                      %msg = new_message( id = lc_message_class number = '035' v1 = ls_return-message_v1 severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.

      IF 1 = 2. MESSAGE e035(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
    ENDIF.
*    ENDIF.

  ENDMETHOD.

  METHOD cba_storepickupordexcptnitem.
  ENDMETHOD.

  METHOD rba_storepickupordexcptnitem.
  ENDMETHOD.

  METHOD cba_storepickupordpickeditem.
  ENDMETHOD.

  METHOD rba_storepickupordpickeditem.
  ENDMETHOD.

  METHOD cba_storepickupordpickingreq.
  ENDMETHOD.

  METHOD rba_storepickupordpickingreq.
  ENDMETHOD.

  METHOD _check_auth_for_withdraw.
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).
    allowed_to_withdraw = abap_false.

    CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to withdraw picking request.
      EXPORTING
        iv_actvt = i_activity
        iv_store = i_store
      IMPORTING
        ev_subrc = DATA(lv_subrc) ).

    CHECK lv_subrc = 0 .
    allowed_to_withdraw = abap_true.

  ENDMETHOD.

*  METHOD _check_auth_for_instance.
*    allowed = abap_false.
*    AUTHORITY-CHECK OBJECT 'V_VBAK_AAT'
*      ID 'ACTVT' FIELD i_activity
*      ID 'AUART' FIELD i_auart.
*
*    DATA(subrc_aat) = sy-subrc.
*
*    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
*      ID 'ACTVT' FIELD i_activity
*      ID 'SPART' FIELD i_spart
*      ID 'VKORG' FIELD i_vkorg
*      ID 'VTWEG' FIELD i_vtweg.
*
*    DATA(subrc_vko) = sy-subrc.
*
*    CHECK subrc_vko = 0 AND subrc_aat = 0.
*    allowed = abap_true.
*  ENDMETHOD.

ENDCLASS.

CLASS lhc_i_storepickuporderpickedit DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE i_storepickuporderpickeditemtp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE i_storepickuporderpickeditemtp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE i_storepickuporderpickeditemtp.

    METHODS read FOR READ
      IMPORTING keys FOR READ i_storepickuporderpickeditemtp RESULT result.

ENDCLASS.

CLASS lhc_i_storepickuporderpickedit IMPLEMENTATION.

  METHOD create.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_i_storepickuporderpickreqt DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    DATA : lo_st_pickreq         TYPE REF TO if_rfm_st_pick_request.
    DATA:lo_st_ord      TYPE REF TO if_rfm_st_pick_order,
         lo_st_ord_util TYPE REF TO if_rfm_st_pick_order_utility.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE i_storepickuporderpickreqtp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE i_storepickuporderpickreqtp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE i_storepickuporderpickreqtp.

    METHODS read FOR READ
      IMPORTING keys FOR READ i_storepickuporderpickreqtp RESULT result.

ENDCLASS.

CLASS lhc_i_storepickuporderpickreqt IMPLEMENTATION.

  METHOD create.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

*******************************
*** Withdraw picking request **
*******************************
  METHOD update.
    DATA : ls_picking_request TYPE rfm_st_pick_req,
           lt_picking_request TYPE rfm_st_pick_req_t,
           lt_return          TYPE tt_bapiret2.
*    lo_st_ord = cl_rfm_st_ord_factory=>get( )->get_st_ord_obj( ).
    lo_st_pickreq = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

    READ TABLE entities ASSIGNING FIELD-SYMBOL(<fs_pickingrequest>) INDEX 1.
    IF sy-subrc = 0.

** Get organizational data
      CALL METHOD lo_st_ord_util->get_vbak_single
        EXPORTING
          iv_vbeln = <fs_pickingrequest>-storepickuporder
        IMPORTING
          es_vbak  = DATA(ls_salesorderheader).

** Authorization Check
      IF lo_st_ord_util->check_authorization( i_activity = '02' i_auart = ls_salesorderheader-auart i_spart = ls_salesorderheader-spart
                                                 i_vkorg = ls_salesorderheader-vkorg i_vtweg = ls_salesorderheader-vtweg ) = abap_true.

        MOVE-CORRESPONDING <fs_pickingrequest> TO ls_picking_request.
        ls_picking_request-referencestorepickuporder = <fs_pickingrequest>-storepickuporder.
        APPEND ls_picking_request TO lt_picking_request.

        CALL METHOD lo_st_pickreq->withdraw_pick_req
          EXPORTING
            it_pp_req = lt_picking_request
          IMPORTING
            et_return = lt_return.

*Message handling
        READ TABLE lt_return INTO DATA(ls_message)  INDEX 1.

        IF sy-subrc = 0.
          CASE ls_message-type.
            WHEN 'E'.
              APPEND VALUE #( store = <fs_pickingrequest>-store storepickuporder = <fs_pickingrequest>-storepickuporder
                              storepickingrequest = <fs_pickingrequest>-storepickingrequest
                              %msg = new_message( id = ls_message-id number = ls_message-number v1 = ls_message-message_v1
                                                  severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickuporderpickreqtp.
            WHEN 'I'.
              APPEND VALUE #( store = <fs_pickingrequest>-store storepickuporder = <fs_pickingrequest>-storepickuporder
                              storepickingrequest = <fs_pickingrequest>-storepickingrequest
                              %msg = new_message( id = ls_message-id number = ls_message-number v1 = ls_message-message_v1
                                                  severity = if_abap_behv_message=>severity-information ) )  TO reported-i_storepickuporderpickreqtp.
            WHEN 'S'.
              APPEND VALUE #( store = <fs_pickingrequest>-store storepickuporder = <fs_pickingrequest>-storepickuporder
                      storepickingrequest = <fs_pickingrequest>-storepickingrequest
                      %msg = new_message( id = ls_message-id number = ls_message-number v1 = ls_message-message_v1
                                          severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickuporderpickreqtp.
            WHEN 'W'.
              APPEND VALUE #( store = <fs_pickingrequest>-store storepickuporder = <fs_pickingrequest>-storepickuporder
                      storepickingrequest = <fs_pickingrequest>-storepickingrequest
                      %msg = new_message( id = ls_message-id number = ls_message-number v1 = ls_message-message_v1
                                          severity = if_abap_behv_message=>severity-warning ) )  TO reported-i_storepickuporderpickreqtp.
          ENDCASE.
        ENDIF.

      ELSE.
        APPEND VALUE #( store = <fs_pickingrequest>-store storepickuporder = <fs_pickingrequest>-storepickuporder
                  storepickingrequest = <fs_pickingrequest>-storepickingrequest
                  %msg = new_message( id = 'RFM_ST_PICK_ORD' number = '036' severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickuporderpickreqtp.
        IF 1 = 2. MESSAGE e036(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_i_storepickupordexcptnitem DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE i_storepickupordexcptnitemtp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE i_storepickupordexcptnitemtp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE i_storepickupordexcptnitemtp.

    METHODS read FOR READ
      IMPORTING keys FOR READ i_storepickupordexcptnitemtp RESULT result.

    METHODS cancelsubstitutedproduct FOR MODIFY IMPORTING it_process_subscancellation
                                                            FOR ACTION i_storepickupordexcptnitemtp~cancelsubstitutedproduct
                                                RESULT    result.

ENDCLASS.

CLASS lhc_i_storepickupordexcptnitem IMPLEMENTATION.

  METHOD create.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD cancelsubstitutedproduct.

    DATA:lo_st_ord        TYPE REF TO if_rfm_st_pick_order,
         lo_pick_req_util TYPE REF TO if_rfm_st_pick_request_utility,
         lo_st_ord_util   TYPE REF TO if_rfm_st_pick_order_utility.
    DATA: lt_store_order TYPE rfm_st_pick_ord_t ,
          is_store_order TYPE rfm_st_pick_ord,
          lt_return      TYPE tt_bapiret2.

    CONSTANTS : lc_message_class           TYPE symsgid VALUE  'RFM_ST_PICK_ORD'.

    ASSERT lines( it_process_subscancellation ) = 1.
    DATA(ls_process_subscancellation) = it_process_subscancellation[ 1 ].

    lo_st_ord = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( ).
    lo_st_ord_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( ).

* Get organizational data
    CALL METHOD lo_st_ord_util->get_vbak_single
      EXPORTING
        iv_vbeln = ls_process_subscancellation-%param-storepickuporder
      IMPORTING
        es_vbak  = DATA(ls_salesorderheader).

** Authorization Check
    IF lo_st_ord_util->check_authorization( i_activity = '02' i_auart = ls_salesorderheader-auart i_spart = ls_salesorderheader-spart
                                            i_vkorg = ls_salesorderheader-vkorg i_vtweg = ls_salesorderheader-vtweg ) = abap_true.

      lo_st_ord->gv_action                   = 'CancelSubstitutedProduct'.
      lo_st_ord->gv_store                    = ls_process_subscancellation-%param-store.
      lo_st_ord->gv_storeorder               = ls_process_subscancellation-%param-storepickuporder .
      lo_st_ord->gv_requested_product        = ls_process_subscancellation-%param-retailstorerequestedproduct.
      lo_st_ord->gv_requested_product_uom    = ls_process_subscancellation-%param-unitofmeasure.
      lo_st_ord->gv_substituted_product      = ls_process_subscancellation-%param-product.
      lo_st_ord->gv_substituted_product_uom  = lo_st_ord->gv_requested_product_uom.

*Get the rejection reason
CALL METHOD lo_st_ord_util->get_rejection_reason_code
 IMPORTING
    ev_rejection_reason_code = lo_st_ord->gv_rejection_reason_code
  .

** Remove substituted product
      CALL METHOD lo_st_ord_util->delete_st_pickorder_subsprod
        EXPORTING
          iv_store              = ls_process_subscancellation-%param-store
          iv_storepickuporder   = ls_process_subscancellation-%param-storepickuporder
          iv_requestedproduct   = ls_process_subscancellation-%param-retailstorerequestedproduct
          iv_substitutedproduct = ls_process_subscancellation-%param-product
          iv_unitofmeasure      = ls_process_subscancellation-%param-unitofmeasure
        IMPORTING
          et_return             = DATA(lt_return_sub).

*Message handling
        READ TABLE lt_return_sub INTO DATA(ls_message)  INDEX 1.

        IF sy-subrc = 0.
          CASE ls_message-type.
            WHEN 'E'.
              APPEND VALUE #( store = ls_process_subscancellation-%param-store storepickuporder = ls_process_subscancellation-%param-storepickuporder
                              %msg = new_message( id = ls_message-id number = ls_message-number v1 = ls_message-message_v1
                                                  severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordexcptnitemtp.
            WHEN 'S'.
              APPEND VALUE #( store = ls_process_subscancellation-%param-store storepickuporder = ls_process_subscancellation-%param-storepickuporder
                      %msg = new_message( id = ls_message-id number = ls_message-number v1 = ls_message-message_v1
                                          severity = if_abap_behv_message=>severity-success ) )  TO reported-i_storepickupordexcptnitemtp.

          ENDCASE.
        ENDIF.

    ELSE.
      APPEND VALUE #( store = ls_process_subscancellation-%param-store
                      storepickuporder = ls_process_subscancellation-%param-storepickuporder
                      %msg = new_message( id = lc_message_class number = '064' severity = if_abap_behv_message=>severity-error ) )  TO reported-i_storepickupordertp.

      IF 1 = 2. MESSAGE e064(rfm_st_pick_ord) INTO DATA(lv_dummy). ENDIF.
    ENDIF.


  ENDMETHOD.

ENDCLASS.

CLASS lsc_i_storepickupordertp DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS check_before_save REDEFINITION.

    METHODS finalize          REDEFINITION.

    METHODS save              REDEFINITION.

ENDCLASS.

CLASS lsc_i_storepickupordertp IMPLEMENTATION.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD finalize.
  ENDMETHOD.

  METHOD save.
    DATA: lo_st_ord TYPE REF TO if_rfm_st_pick_order.
    lo_st_ord = cl_rfm_st_pick_order_factory=>get( )->get_st_ord_obj( ).

    CASE lo_st_ord->gv_action.

* Perform of 2nd step PGI , Billing
      WHEN 'ProcessOrderHandover'.

        CALL METHOD lo_st_ord->process_pgi_invoice_of_stord
          EXPORTING
            iv_store      = lo_st_ord->gv_store
            iv_storeorder = lo_st_ord->gv_storeorder.

* Reversal of 1st step PGI , Delete delivery , cancel SO (Reservation Hand over)
      WHEN 'ProcessResvnOrderHndovr'.


        CALL METHOD lo_st_ord->reversal_of_stord_when_rho
          EXPORTING
            iv_store      = lo_st_ord->gv_store
            iv_storeorder = lo_st_ord->gv_storeorder.
*            iv_rejection_reason_code = lo_st_ord->gv_rejection_reason_code_rh.

* Reversal of 1st step PGI , Delete delivery , cancel SO ( Cancel Store Order)
      WHEN 'ProcessStoreOrderCancellation'.

        CALL METHOD lo_st_ord->reversal_of_stord_when_cancel
          EXPORTING
            iv_store                 = lo_st_ord->gv_store
            iv_storeorder            = lo_st_ord->gv_storeorder
            iv_rejection_reason_code = lo_st_ord->gv_rejection_reason_code
            iv_comment               = lo_st_ord->gv_rejection_comments.

*Cancelling the Substituted Product
      WHEN 'CancelSubstitutedProduct'.

        CALL METHOD lo_st_ord->cancel_substitute
          EXPORTING
            iv_store                   = lo_st_ord->gv_store
            iv_sales_order             = lo_st_ord->gv_storeorder
            iv_requested_product       = lo_st_ord->gv_requested_product
            iv_requested_product_uom   = lo_st_ord->gv_requested_product_uom
            iv_substituted_product     = lo_st_ord->gv_substituted_product
            iv_substituted_product_uom = lo_st_ord->gv_substituted_product_uom
            iv_rejection_reason_code   = lo_st_ord->gv_rejection_reason_code .
*      RECEIVING
*              rt_messages            =
*             .

    ENDCASE.

  ENDMETHOD.



ENDCLASS.