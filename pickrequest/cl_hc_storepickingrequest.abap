CLASS lhc_i_storepickingrequest DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA: lo_pick_req TYPE REF TO if_rfm_st_pick_request.
    DATA: lo_pick_req_util  TYPE REF TO if_rfm_st_pick_request_utility.
    DATA: lv_error_buffer TYPE abap_boolean.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK i_storepickingrequesttp.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE i_storepickingrequesttp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE i_storepickingrequesttp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE i_storepickingrequesttp.

    METHODS read FOR READ
      IMPORTING keys FOR READ i_storepickingrequesttp RESULT result.

    METHODS cba_items FOR MODIFY
      IMPORTING entities_cba FOR CREATE i_storepickingrequesttp\_items.

    METHODS printpickingreqcontent FOR MODIFY IMPORTING it_requests FOR ACTION i_storepickingrequesttp~printpickingreqcontent.

    METHODS takeoverpickingrequests FOR MODIFY IMPORTING it_requests FOR ACTION i_storepickingrequesttp~takeoverpickingrequests.

    METHODS releasepickingrequests FOR MODIFY IMPORTING it_requests FOR ACTION i_storepickingrequesttp~releasepickingrequests.

    METHODS transferpickingrequests FOR MODIFY IMPORTING it_requests FOR ACTION i_storepickingrequesttp~transferpickingrequests.

    METHODS processproductstndrdid FOR MODIFY IMPORTING it_requests FOR ACTION i_storepickingrequesttp~processprodwithalternateuom RESULT result.

    METHODS rba_items FOR READ
      IMPORTING keys_rba FOR READ i_storepickingrequesttp\_items FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_i_storepickingrequest IMPLEMENTATION.

  METHOD lock.
    SET LOCKS OF i_storepickingrequesttp ENTITY i_storepickingrequestitemtp
    FROM VALUE #( FOR keyval IN keys ( store = keyval-store storepickingrequest = keyval-storepickingrequest ) )
    FAILED DATA(it_failed)
    REPORTED DATA(it_reported).

    failed-i_storepickingrequesttp = VALUE #( FOR failed_request
      IN it_failed-i_storepickingrequesttp ( %cid    = failed_request-%cid
                                             %key    = failed_request-%key
                                             %fail   = failed_request-%fail
                                             %create = failed_request-%create
                                             %update = failed_request-%update
                                             %delete = failed_request-%delete
                                             %action = failed_request-%action ) ).

    reported-i_storepickingrequesttp = VALUE #( FOR reported_request
      IN it_reported-i_storepickingrequesttp ( %cid        = reported_request-%cid
                                               %key        = reported_request-%key
                                               %msg        = reported_request-%msg
                                               %element    = reported_request-%element
                                               %state_area = reported_request-%state_area ) ).
  ENDMETHOD.

  METHOD create.

    DATA lt_pp_req TYPE rfm_st_pick_req_t.
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF lo_pick_req_util IS NOT BOUND.
      RETURN.
    ENDIF.
    MOVE-CORRESPONDING entities TO lt_pp_req.

    lo_pick_req_util->insert_st_pick_req_head(
       EXPORTING
         it_rtst_pp_request = lt_pp_req               " Table type for structure RTST_PP_ORDERS
      IMPORTING
      et_return      =  DATA(lt_return)                " Table Type for BAPIRET2
      ).
    IF lt_return IS NOT INITIAL.

    ENDIF.

  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.

    DATA: lt_rfm_st_pick_request TYPE rfm_st_pick_req_t,
          lt_return              TYPE if_rfm_st_pick_request=>tt_behv_return,
          lv_subrc               TYPE sy-subrc.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF entities IS NOT INITIAL.
      DATA(lt_entities) = entities.
      SORT lt_entities BY store.
      LOOP AT lt_entities INTO DATA(ls_entities).

        IF ( sy-tabix GT 1 AND lt_entities[ sy-tabix - 1 ]-store EQ ls_entities-store ).
          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
                                            store  = ls_entities-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequesttp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_entities-store
          IMPORTING
            ev_subrc = lv_subrc ).

        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
                                            store  = ls_entities-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequesttp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.

      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_rfm_st_pick_request = CORRESPONDING #( entities MAPPING handovershelfspaceid = handovershelfspaceid
                                                                               store = store
                                                            storepickingrequest = storepickingrequest ).
      IF lt_rfm_st_pick_request IS NOT INITIAL.
        CALL METHOD lo_pick_req->assign_storage(
          EXPORTING
            it_pp_req = lt_rfm_st_pick_request
          IMPORTING
            et_return = lt_return ).
      ENDIF.
      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest      = ls_return-storepickingrequestid
                                          store    = ls_return-store
                                          %msg     = new_message( id = ls_return-id
                                                            number   = ls_return-number
                                                            severity = ls_return-type
                                                            v1       = ls_return-msgv1
                                                            v2       = ls_return-msgv2
                                                            ) ) TO reported-i_storepickingrequesttp.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD printpickingreqcontent.

    DATA: lt_pp_requests TYPE rtst_pkr_print_t,
          lt_return      TYPE if_rfm_st_pick_request=>tt_behv_return,
          lv_subrc       TYPE sy-subrc.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF it_requests IS NOT INITIAL.

      DATA(lt_requests) = it_requests.
      SORT lt_requests BY store.

      LOOP AT lt_requests INTO DATA(ls_requests). " Put this method inside the begin picking loop and proceed with valid stores.

        IF ( sy-tabix GT 1 AND lt_requests[ sy-tabix - 1 ]-store EQ ls_requests-store ).

          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                              store  = ls_requests-store
                                              %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                                number   = 003
                                                                severity = if_abap_behv_message=>severity-error
                                                                ) ) TO reported-i_storepickingrequesttp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_requests-store
          IMPORTING
            ev_subrc = lv_subrc ).

        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                            store  = ls_requests-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequesttp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.
      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_pp_requests = CORRESPONDING #( it_requests MAPPING storepickingrequest = storepickingrequest
      store = store
      customer  = %param-customer
      contentlist = %param-contentlistprintistriggered
      manualprint = %param-manualprintistriggered ).

      CALL METHOD lo_pick_req->begin_picking          "Modularize this method and call inside BADI
        EXPORTING
          it_rtst_pkr_print = lt_pp_requests
        IMPORTING
          et_return         = lt_return.

      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest = ls_return-storepickingrequestid
          store = ls_return-store
          %msg = new_message( id = ls_return-id
           number = ls_return-number
           severity = ls_return-type
           v1       = ls_return-msgv1
           v2       = ls_return-msgv2
         ) ) TO reported-i_storepickingrequesttp.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD takeoverpickingrequests.

    DATA: lt_return   TYPE if_rfm_st_pick_request=>tt_behv_return,
          lt_pick_req TYPE rfm_st_pick_req_t,
          lv_subrc    TYPE sy-subrc.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF it_requests IS NOT INITIAL.

      DATA(lt_requests) = it_requests.
      SORT lt_requests BY store.

      LOOP AT lt_requests INTO DATA(ls_requests).

        IF ( sy-tabix GT 1 AND lt_requests[ sy-tabix - 1 ]-store EQ ls_requests-store ).

          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                              store  = ls_requests-store
                                              %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                                number   = 003
                                                                severity = if_abap_behv_message=>severity-error
                                                                ) ) TO reported-i_storepickingrequesttp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_requests-store
          IMPORTING
            ev_subrc = lv_subrc ).

        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                            store  = ls_requests-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequesttp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.
      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_pick_req = CORRESPONDING #( it_requests MAPPING storepickingrequest = storepickingrequest store = store ).

      CALL METHOD lo_pick_req->takeover_pick_request
        EXPORTING
          it_takeover_req = lt_pick_req
        IMPORTING
          et_return       = lt_return.

      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest = ls_return-storepickingrequestid
          store = ls_return-store
          %msg = new_message( id = ls_return-id
           number = ls_return-number
           severity = ls_return-type
           v1       = ls_return-msgv1
           v2       = ls_return-msgv2
         ) ) TO reported-i_storepickingrequesttp.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD releasepickingrequests.

    DATA: lt_return   TYPE if_rfm_st_pick_request=>tt_behv_return,
          lt_pick_req TYPE rfm_st_pick_req_t,
          lv_subrc    TYPE sy-subrc.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF it_requests IS NOT INITIAL.

      DATA(lt_requests) = it_requests.
      SORT lt_requests BY store.

      LOOP AT lt_requests INTO DATA(ls_requests).

        IF ( sy-tabix GT 1 AND lt_requests[ sy-tabix - 1 ]-store EQ ls_requests-store ).

          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                              store  = ls_requests-store
                                              %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                                number   = 003
                                                                severity = if_abap_behv_message=>severity-error
                                                                ) ) TO reported-i_storepickingrequesttp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_requests-store
          IMPORTING
            ev_subrc = lv_subrc ).

        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                            store  = ls_requests-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequesttp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.
      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_pick_req = CORRESPONDING #( it_requests MAPPING storepickingrequest = storepickingrequest store = store ).

      CALL METHOD lo_pick_req->release_pick_request
        EXPORTING
          it_release_req = lt_pick_req
        IMPORTING
          et_return      = lt_return.

      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest = ls_return-storepickingrequestid
          store = ls_return-store
          %msg = new_message( id = ls_return-id
           number = ls_return-number
           severity = ls_return-type
           v1       = ls_return-msgv1
           v2       = ls_return-msgv2
         ) ) TO reported-i_storepickingrequesttp.
        ENDLOOP.
      ENDIF.

    ENDIF.


  ENDMETHOD.

  METHOD transferpickingrequests.

    DATA: lt_return   TYPE if_rfm_st_pick_request=>tt_behv_return,
          lt_pick_req TYPE rfm_st_pick_req_t,
          lv_subrc    TYPE sy-subrc.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF it_requests IS NOT INITIAL.

      DATA(lt_requests) = it_requests.
      SORT lt_requests BY store.

      LOOP AT lt_requests INTO DATA(ls_requests).

        IF ( sy-tabix GT 1 AND lt_requests[ sy-tabix - 1 ]-store EQ ls_requests-store ).

          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                              store  = ls_requests-store
                                              %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                                number   = 003
                                                                severity = if_abap_behv_message=>severity-error
                                                                ) ) TO reported-i_storepickingrequesttp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_requests-store
          IMPORTING
            ev_subrc = lv_subrc ).

        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_requests-storepickingrequest
                                            store  = ls_requests-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequesttp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.
      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_pick_req = CORRESPONDING #( it_requests MAPPING storepickingrequest = storepickingrequest
                                                         store = store
                                                         userassignedtopickingrequest = %param-userid ).

      CALL METHOD lo_pick_req->transfer_pick_request
        EXPORTING
          it_transfer_req = lt_pick_req
        IMPORTING
          et_return       = lt_return.

      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest = ls_return-storepickingrequestid
          store = ls_return-store
          %msg = new_message( id = ls_return-id
           number = ls_return-number
           severity = ls_return-type
           v1       = ls_return-msgv1
           v2       = ls_return-msgv2
         ) ) TO reported-i_storepickingrequesttp.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD processproductstndrdid.

    IF it_requests IS NOT INITIAL.

      lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).

      DATA: lv_productstandardid TYPE ean11,
            lv_weight            TYPE brgew_ap,
            lv_weightunit        TYPE gewei,
            lv_price             TYPE endbp,
            lv_currencycode      TYPE waerk.

      LOOP AT it_requests INTO DATA(ls_requests).

        CALL METHOD lo_pick_req->process_gtin
          EXPORTING
            iv_scanned_gtin  = ls_requests-%param-productstandardid
          IMPORTING
            ev_gtin          = lv_productstandardid
            ev_weight        = lv_weight
            ev_weight_unit   = lv_weightunit
            ev_price         = lv_price
            ev_currency_unit = lv_currencycode.

        APPEND VALUE #( storepickingrequest = ls_requests-storepickingrequest store = ls_requests-store  %param-productstandardid = lv_productstandardid
                       %param-netweight = lv_weight %param-weightunit = lv_weightunit  %param-netproductprice = lv_price  %param-transactioncurrency = lv_currencycode ) TO result.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD cba_items.

    DATA: lt_pp_req_item TYPE rfm_st_pick_reqi_t,
          ls_pp_req_item TYPE  rfm_st_pick_reqi.
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF lo_pick_req_util IS NOT BOUND.
      RETURN.
    ENDIF.
    LOOP AT entities_cba INTO DATA(ls_entities).
      LOOP AT ls_entities-%target INTO DATA(ls_item).
        MOVE-CORRESPONDING ls_item TO ls_pp_req_item.
        APPEND ls_pp_req_item TO lt_pp_req_item.
      ENDLOOP.
    ENDLOOP.
    lo_pick_req_util->insert_st_pick_req_item(
       EXPORTING
         it_rtst_pp_req_item = lt_pp_req_item              " Table type for structure RTST_PP_ORDERS
      IMPORTING
      et_return      =  DATA(lt_return)                " Table Type for BAPIRET2
      ).
    IF lt_return IS NOT INITIAL.

    ENDIF.

  ENDMETHOD.

  METHOD rba_items.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_i_storepickingrequestitem DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA: lo_pick_req TYPE REF TO if_rfm_st_pick_request.
    DATA: lo_pick_req_util  TYPE REF TO if_rfm_st_pick_request_utility.
    DATA: lv_error_buffer TYPE abap_boolean.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE i_storepickingrequestitemtp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE i_storepickingrequestitemtp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE i_storepickingrequestitemtp.

    METHODS read FOR READ
      IMPORTING keys FOR READ i_storepickingrequestitemtp RESULT result.

ENDCLASS.

CLASS lhc_i_storepickingrequestitem IMPLEMENTATION.

  METHOD create.

    DATA: lt_pp_req_item   TYPE rfm_st_pick_reqi_t,
          lt_return        TYPE if_rfm_st_pick_request=>tt_behv_return,
          lv_subrc         TYPE sy-subrc.
*          ls_req_itm       TYPE rfm_st_pick_reqi,
*          lt_pick_itm      TYPE rfm_st_pick_reqi_t,
*          lt_upd_itm       TYPE rfm_st_pick_reqi_t,
*          lv_itm_num       TYPE rfm_st_pick_request_item,
*          lt_items_uom     TYPE rfm_st_pick_reqi_t,
*          lt_uom           TYPE if_rfm_st_pick_request_utility=>tt_uom,
*          lv_qty_sum       TYPE rfm_st_pick_qty,
*          lv_tolerance_up  TYPE rfm_st_pick_qty,
*          lv_tolerance_low TYPE rfm_st_pick_qty.
*
*    DATA : lv_limit_up   TYPE uebto_di VALUE '0.05'.
*    DATA : lv_limit_low  TYPE untto_di VALUE '0.05'.

    IF entities IS NOT INITIAL.

      DATA(lt_entities) = entities.
      SORT lt_entities BY store.

      lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
      lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

      LOOP AT entities INTO DATA(ls_entities).
        IF ( sy-tabix GT 1 AND lt_entities[ sy-tabix - 1 ]-store EQ ls_entities-store ).
          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
                                            store  = ls_entities-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequestitemtp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_entities-store
          IMPORTING
            ev_subrc = lv_subrc ).
        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
                                            store  = ls_entities-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequestitemtp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.

      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_pp_req_item = CORRESPONDING #( entities MAPPING storepickingrequest           = storepickingrequest
                                                         store                         = store
                                                         storepickingrequestitem       = storepickingrequestitem
                                                         product                       = product
                                                         requestedquantity             = requestedquantity
                                                         productpickedquantity         = productpickedquantity
                                                         unitofmeasure                 = unitofmeasure
                                                         storepickingrequestitemstatus = storepickingrequestitemstatus
                                                         referencestorepickuporder     = referencestorepickuporder
                                                         referencestorepickuporderitem = referencestorepickuporderitem
                                                         storeitemoverallstatus        = storeitemoverallstatus
                                                         requestedstorepickingreqitem  = requestedstorepickingreqitem
                                                         storepkngreqitemissubstituted = storepkngreqitemissubstituted
                                                         retailstoreprodispickingcmpltd = retailstoreprodispickingcmpltd
                                                         ).

      CALL METHOD lo_pick_req->create_picking_item(
        EXPORTING
          it_pp_req_item = lt_pp_req_item
        IMPORTING
          et_return      = lt_return ).

      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest   = ls_return-storepickingrequestid
                        storepickingrequestitem = ls_return-storepickingrequestitem
                                          store = ls_return-store
                                           %msg =  new_message( id = ls_return-id
                                                          number   = ls_return-number
                                                          severity = ls_return-type
                                                          v1       = ls_return-msgv1
                                                          v2       = ls_return-msgv2
                                                            ) ) TO reported-i_storepickingrequestitemtp.
        ENDLOOP.
      ENDIF.

*      LOOP AT entities INTO ls_entities.
*        MOVE-CORRESPONDING ls_entities TO ls_req_itm.
*        SELECT * FROM rfm_st_pick_reqi INTO TABLE @lt_pick_itm WHERE store = @ls_req_itm-store
*                                                        AND storepickingrequest = @ls_req_itm-storepickingrequest.
*        IF sy-subrc = 0.
*          READ TABLE lt_pick_itm INTO DATA(ls_pick_itm) WITH KEY store = ls_req_itm-store
*                                                                 storepickingrequest = ls_req_itm-storepickingrequest
*                                                                 storepickingrequestitem = ls_req_itm-requestedstorepickingreqitem.
*
*          " Read Table is for finding the Parent Item
*
*          IF sy-subrc = 0.
*
*            APPEND ls_pick_itm TO lt_items_uom.
*
*            CALL METHOD lo_pick_req_util->read_uom_data(
*              EXPORTING
*                it_item_tab = lt_items_uom
*              IMPORTING
*                et_uom      = lt_uom ).
*
*            READ TABLE lt_uom INTO DATA(ls_uom) WITH KEY unitofmeasure = ls_pick_itm-unitofmeasure.
*            IF sy-subrc = 0.
*              IF NOT ( ls_uom-unitofmeasuredspnmbrofdcmls IS INITIAL AND ls_uom-unitofmeasurenumberofdecimals IS INITIAL ).
*
*                lv_tolerance_up = ls_pick_itm-requestedquantity * lv_limit_up.
*                lv_tolerance_low = ls_pick_itm-requestedquantity * lv_limit_low.
*                LOOP AT lt_pick_itm INTO DATA(ls_pick_itm_sum_dcml) WHERE store = ls_pick_itm-store
*                                                                       AND storepickingrequest = ls_pick_itm-storepickingrequest
*                                                                       AND requestedstorepickingreqitem = ls_pick_itm-storepickingrequestitem.
*
*                  lv_qty_sum  = lv_qty_sum + ls_pick_itm_sum_dcml-productpickedquantity.
*
*                ENDLOOP.
*
*                lv_qty_sum  = lv_qty_sum + ls_pick_itm-productpickedquantity + ls_req_itm-productpickedquantity.
*                lv_tolerance_up = ls_pick_itm-requestedquantity + lv_tolerance_up.
*                lv_tolerance_low = ls_pick_itm-requestedquantity - lv_tolerance_up.
*
*                IF lv_qty_sum GE ls_pick_itm-requestedquantity AND lv_qty_sum LE lv_tolerance_up.
*                  ls_req_itm-storepickingrequestitem = ( lines( lt_pick_itm ) + 1 )  * 10.
*                  ls_pick_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storeitemoverallstatus = 'E'.
*                  APPEND ls_req_itm TO lt_upd_itm.
*                  APPEND ls_pick_itm TO lt_upd_itm.
*
*                  APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
*                                            store  = ls_entities-store
*                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
*                                                              number   = 003
*                                                              severity = if_abap_behv_message=>severity-success
*                                                              ) ) TO reported-i_storepickingrequestitemtp.
*
*                ELSEIF lv_qty_sum LT ls_pick_itm-requestedquantity AND lv_qty_sum GE lv_tolerance_low.
*                  ls_req_itm-storepickingrequestitem = ( lines( lt_pick_itm ) + 1 )  * 10.
*                  ls_pick_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storeitemoverallstatus = 'E'.
*                  APPEND ls_req_itm TO lt_upd_itm.
*                  APPEND ls_pick_itm TO lt_upd_itm.
*
*                  APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
*                                            store  = ls_entities-store
*                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
*                                                              number   = 003
*                                                              severity = if_abap_behv_message=>severity-success
*                                                              ) ) TO reported-i_storepickingrequestitemtp.
*
*                ELSEIF lv_qty_sum LT lv_tolerance_low.
*
*                  ls_req_itm-storepickingrequestitem = ( lines( lt_pick_itm ) + 1 )  * 10.
*                  ls_pick_itm-storepickingrequestitemstatus = 'B'.
*                  ls_req_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storeitemoverallstatus = 'E'.
*                  APPEND ls_req_itm TO lt_upd_itm.
*                  APPEND ls_pick_itm TO lt_upd_itm.
*
*                ELSE.
*                  "Exceeded Error
*
*                  APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
*                                            store  = ls_entities-store
*                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
*                                                              number   = 003
*                                                              severity = if_abap_behv_message=>severity-success
*                                                              ) ) TO reported-i_storepickingrequestitemtp.
*                ENDIF.
*
*                " decimal relv
*              ELSE.
*                LOOP AT lt_pick_itm INTO DATA(ls_pick_itm_sum) WHERE store = ls_pick_itm-store
*                                                                     AND storepickingrequest = ls_pick_itm-storepickingrequest
*                                                                     AND requestedstorepickingreqitem = ls_pick_itm-storepickingrequestitem.
*
*                  lv_qty_sum  = lv_qty_sum + ls_pick_itm_sum-productpickedquantity.
*
*                ENDLOOP.
*
*                lv_qty_sum  = lv_qty_sum + ls_pick_itm-productpickedquantity + ls_req_itm-productpickedquantity.
*
*                IF lv_qty_sum EQ ls_pick_itm-requestedquantity.
*                  ls_req_itm-storepickingrequestitem = ( lines( lt_pick_itm ) + 1 )  * 10.
*                  ls_pick_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storeitemoverallstatus = 'E'.
*                  APPEND ls_req_itm TO lt_upd_itm.
*                  APPEND ls_pick_itm TO lt_upd_itm.
*
*                  APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
*                                            store  = ls_entities-store
*                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
*                                                              number   = 003
*                                                              severity = if_abap_behv_message=>severity-success
*                                                              ) ) TO reported-i_storepickingrequestitemtp.
*
*                ELSEIF lv_qty_sum LT ls_pick_itm-requestedquantity.
*                  ls_req_itm-storepickingrequestitem = ( lines( lt_pick_itm ) + 1 )  * 10.
*                  ls_pick_itm-storepickingrequestitemstatus = 'B'.
*                  ls_req_itm-storepickingrequestitemstatus = 'A'.
*                  ls_req_itm-storeitemoverallstatus = 'E'.
*                  APPEND ls_req_itm TO lt_upd_itm.
*                  APPEND ls_pick_itm TO lt_upd_itm.
*
*                  APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
*                                            store  = ls_entities-store
*                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
*                                                              number   = 003
*                                                              severity = if_abap_behv_message=>severity-success
*                                                              ) ) TO reported-i_storepickingrequestitemtp.
*                ELSE.
*                  "Append Exceeded Error here
*
*                  APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
*                                            store  = ls_entities-store
*                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
*                                                              number   = 003
*                                                              severity = if_abap_behv_message=>severity-success
*                                                              ) ) TO reported-i_storepickingrequestitemtp.
*
*                ENDIF.
*                CLEAR : lv_qty_sum.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*      ENDLOOP.
*
    ENDIF.

*    IF lt_upd_itm IS NOT INITIAL.
*      MODIFY rfm_st_pick_reqi FROM TABLE lt_upd_itm.
*    ENDIF.

  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.

    DATA: lt_pp_req_item TYPE rfm_st_pick_reqi_t,
          lt_return      TYPE if_rfm_st_pick_request=>tt_behv_return,
          lv_subrc       TYPE sy-subrc.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
    lo_pick_req_util = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr_utility( ).

    IF entities IS NOT INITIAL.

      DATA(lt_entities) = entities.
      SORT lt_entities BY store.

      LOOP AT entities INTO DATA(ls_entities).
        IF ( sy-tabix GT 1 AND lt_entities[ sy-tabix - 1 ]-store EQ ls_entities-store ).
          IF lv_error_buffer EQ abap_true.
            APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
                                            store  = ls_entities-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequestitemtp.
          ENDIF.
          CONTINUE.
        ENDIF.

        CALL METHOD lo_pick_req_util->authorization_check(       " Authorization check to change picking request.
          EXPORTING
            iv_actvt = 02
            iv_store = ls_entities-store
          IMPORTING
            ev_subrc = lv_subrc ).
        IF lv_subrc NE 0.
          APPEND VALUE #( storepickingrequest      = ls_entities-storepickingrequest
                                            store  = ls_entities-store
                                            %msg   = new_message( id   = 'RFM_ST_PICK_REQ'
                                                              number   = 003
                                                              severity = if_abap_behv_message=>severity-error
                                                              ) ) TO reported-i_storepickingrequestitemtp.
          lv_error_buffer = abap_true.
        ELSE.
          lv_error_buffer = abap_false.
        ENDIF.
      ENDLOOP.

      IF reported IS NOT INITIAL.
        RETURN.
      ENDIF.

      lt_pp_req_item = CORRESPONDING #( entities MAPPING storepickingrequest  = storepickingrequest
                                                        storepickingrequestitem = storepickingrequestitem
                                                        productpickedquantity   = productpickedquantity
                                                        store                   = store
                                                        retailstoreprodispickingcmpltd = retailstoreprodispickingcmpltd ).
      CALL METHOD lo_pick_req->update_picking_item(
        EXPORTING
          it_pp_req_item = lt_pp_req_item
        IMPORTING
          et_return      = lt_return ).

      IF lt_return IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return).
          APPEND VALUE #( storepickingrequest   = ls_return-storepickingrequestid
                        storepickingrequestitem = ls_return-storepickingrequestitem
                                          store = ls_return-store
                                           %msg =  new_message( id = ls_return-id
                                                          number   = ls_return-number
                                                          severity = ls_return-type
                                                          v1       = ls_return-msgv1
                                                          v2       = ls_return-msgv2
                                                            ) ) TO reported-i_storepickingrequestitemtp.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_i_storepickingrequest DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS check_before_save REDEFINITION.

    METHODS finalize  REDEFINITION.

    METHODS save  REDEFINITION.

    METHODS cleanup REDEFINITION.

ENDCLASS.

CLASS lsc_i_storepickingrequest IMPLEMENTATION.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD finalize.
  ENDMETHOD.

  METHOD save.

    DATA: lo_pick_req TYPE REF TO if_rfm_st_pick_request.

    lo_pick_req = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( ).
* Push the code to BADi
    IF lo_pick_req->gv_action = 'FinishPicking'.
      CALL METHOD lo_pick_req->finish_picking( ).
      CLEAR: lo_pick_req->gv_action, lo_pick_req->gt_fnsh_pick_itm, lo_pick_req->gv_picking_request.
    ENDIF.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

ENDCLASS.