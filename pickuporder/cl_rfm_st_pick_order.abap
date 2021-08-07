CLASS cl_rfm_st_pick_order DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS cl_rfm_st_pick_order_factory .

  PUBLIC SECTION.

    INTERFACES if_rfm_st_pick_order .

    CLASS-DATA gv_action_from_api TYPE abap_bool .
    CLASS-DATA gv_call_update_task TYPE abap_bool .

    METHODS constructor
      IMPORTING
        !io_application_log TYPE REF TO cl_rtst_application_log OPTIONAL .
  PROTECTED SECTION.

    DATA go_application_log TYPE REF TO cl_rtst_application_log .
private section.

  class-data MS_VBAK type VBAK .
  class-data MT_VBAP type VBAP_T .
  data MO_ST_PKR type ref to IF_RFM_ST_PICK_REQUEST .
  data MO_ST_ORD_UTIL type ref to IF_RFM_ST_PICK_ORDER_UTILITY .
  class-data MS_C_PP_STORE type RFM_C_PICK_ORD .
  class-data MT_VBEP type VBEP_T .
  class-data MV_PAYMENT_STATUS type RFM_ST_ORDER_PAYMENT_STATUS .

  methods CHECK_ORDER_TYPE
    importing
      !REF_ORD_NUM type VBELN optional
      !IS_VBAK type VBAK optional
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods CREATE_FROM_SD_ORDER
    importing
      !IT_VBAP type VBAP_T optional
      !IT_VBEP type VBEP_T optional
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods CREATE_STORE_ORDER
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  class-methods CREATE
    returning
      value(RO_ST_ORD) type ref to IF_RFM_ST_PICK_ORDER .
  methods UPDATE_STORE_ORDER
    importing
      !IT_STORE_ORDER type RFM_ST_PICK_ORD_T
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods SET_OBJECTS .
  methods DELETE_STORE_ORDER
    importing
      !IV_ST_ORDER type RFM_ST_PICKUP_ORDER optional
      !IT_ST_ORDER type RFM_ST_PICK_ORD_T optional
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods PREPARE_STORE_ORDER
    exporting
      !ET_STORE_ORDER type RFM_ST_PICK_ORD_T
      !ET_RETURN type TT_BAPIRET2 .
  methods PREPARE_STORE_PKR_REQUEST
    exporting
      !ET_ORDER_DETAILS type IF_RFM_ST_PICK_ORD_SPLIT_PROD=>TT_ORDER_DET
      !ET_ORDER_HEADER type IF_RFM_ST_PICK_ORD_SPLIT_PROD=>TT_ORDER_HEADER .
ENDCLASS.



CLASS CL_RFM_ST_PICK_ORDER IMPLEMENTATION.


  METHOD check_order_type.

    DATA ls_vbak TYPE vbak.

    IF is_vbak IS INITIAL.
      mo_st_ord_util->get_vbak_single(
   EXPORTING
       iv_vbeln = ref_ord_num
     IMPORTING
       es_vbak = ls_vbak
       et_return = DATA(lt_return)
   ).
      IF lt_return IS NOT INITIAL.
        APPEND LINES OF lt_return TO et_return.
      ENDIF.
    ELSE.
      ls_vbak = is_vbak.
    ENDIF.

    IF sy-subrc = 0.
      IF ls_vbak IS NOT INITIAL. " this makes sure this is not an STO, but still need to check that this is a SD sales order
        IF ls_vbak-vbtyp = 'C' .
          MOVE-CORRESPONDING ls_vbak TO ms_vbak.
*        ELSE.
*          " raise an exception that this cant be a sales order.
        ENDIF.
      ENDIF.
*    ELSEIF sy-subrc <> 0.
      " here check for the STO type, as we didnt find anything for the order number in Sales order table.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    go_application_log = cl_rtst_application_log=>get_instance( ).
  ENDMETHOD.


  METHOD create.
    ro_st_ord = NEW cl_rfm_st_pick_order( ).
  ENDMETHOD.


  METHOD create_from_sd_order.

    DATA: lt_vbap        TYPE TABLE OF vbap,
          ls_vbap        TYPE vbap,
          ls_t001w       TYPE t001w,
          lv_vstel       TYPE vstel,
          ls_return      TYPE bapiret2,
          lt_product     TYPE TABLE OF pre03,
          lo_rfm_idt     TYPE REF TO rfm_st_pick_ord_ident_mod,
          lt_vbap_badi   TYPE TABLE OF vbap,
          ls_vbak_badi   TYPE vbak,
          lt_vbep_badi   TYPE TABLE OF vbep,
          ls_cnc_ord     TYPE boolean_flg,
          lv_impl_active TYPE boolean_flg,
          lt_return_badi TYPE tt_bapiret2.
    " first check if the plant for the given sales order is a store, then check if all the plants/stores for all the items in the sales order are the same.
    CHECK ms_vbak IS NOT INITIAL.


    IF it_vbap IS INITIAL .
      CALL METHOD mo_st_ord_util->get_vbap
        EXPORTING
          iv_vbeln = ms_vbak-vbeln
        IMPORTING
          et_vbap  = lt_vbap.
    ELSE.
      lt_vbap[] = it_vbap[].
    ENDIF.

    IF lt_vbap IS NOT INITIAL. " CHECK IF LT_VBAP HAS SOME VALUE...BEGINS HERE

      READ TABLE lt_vbap INTO ls_vbap INDEX 1.
      IF sy-subrc = 0.
        CALL METHOD mo_st_ord_util->get_t001w_single
          EXPORTING
            iv_werks = ls_vbap-werks
          IMPORTING
            es_t001w = ls_t001w.
        IF ls_t001w IS NOT INITIAL.  " CHECK IF T0001W HAS SOME VALUE...BEGINS HERE
          IF ls_t001w-vlfkz <> 'A'.   " check for store begins here
            "should we raise a message here??...
            "no further processing
            ls_return-id = 'RFM_ST_PICK_ORD'.
            ls_return-type = 'E'.
            ls_return-number = '013'.
            ls_return-message_v1 = ls_vbap-vbeln.
            ls_return-message_v3 = ls_vbap-werks.
            SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
            MESSAGE e013(rfm_st_pick_ord)
        INTO ls_return-message
        WITH ls_vbap-vbeln.
            APPEND ls_return TO et_return.
            RETURN.
          ENDIF.  " check for store ends here
*        ELSEIF sy-subrc <> 0.
*     Implement suitable error handling here for T001W_SINGLE_READ
        ENDIF.  "CHECK IF T0001W HAS SOME VALUE...ENDS HERE

        IF it_vbep IS INITIAL.
          mo_st_ord_util->get_vbep_with_posnr(
            EXPORTING
              i_vbeln =  ls_vbap-vbeln                " Sales Document
            IMPORTING
              et_vbep =  mt_vbep[]      " Table Type for SD Schedule Line
              et_return = et_return
          ).
        ELSE.
          mt_vbep[] = it_vbep[].
        ENDIF.
      ENDIF.

*    ELSEIF sy-subrc <> 0.
* Implement suitable error handling here for SD_VBAP_READ_WITH_VBELN
    ENDIF. " CHECK IF LT_VBAP HAS SOME VALUE...ENDS HERE
    " we need to check for shipping point customizing and need to check whether the used shipping point is same as the one maintained in the customizing.
    " read customizing for shipping point
* BADI to get the custom identification criteria
    TRY .
        GET BADI lo_rfm_idt.
        CLEAR lv_impl_active.
        IF lo_rfm_idt IS BOUND.
          ls_vbak_badi = ms_vbak.
          lt_vbap_badi = lt_vbap.
          lt_vbep_badi = mt_vbep.

          CALL BADI lo_rfm_idt->modify_st_ord_identification
            EXPORTING
              so_header_in     = ls_vbak_badi                 " Sales Document: Header Data
              so_item_in       = lt_vbap_badi                 " Sales Document: Item Data
              so_sched_in      = lt_vbep_badi                 " Sales Document: Schedule Line Data
            CHANGING
              is_store_ord_out = ls_cnc_ord                 " Boolean Variables (X=true, space=false)
              et_return        = lt_return_badi.
          lv_impl_active = abap_true.
          IF lt_return_badi is NOT INITIAL.
            APPEND LINES OF lt_return_badi to et_return.
          ENDIF.
          CHECK ls_cnc_ord IS NOT INITIAL.
        ELSE.
          lv_impl_active = abap_false.
        ENDIF.
      CATCH cx_ble_runtime_error. " BLE: Runtime Error
    ENDTRY.



    lt_product = CORRESPONDING #( lt_vbap MAPPING matnr = matnr ).

    mo_st_ord_util->get_mara_array(
      EXPORTING
        it_pre03  =   lt_product               " PRE03 Table
      IMPORTING
        et_mara   =  DATA(lt_mara)             " Table type for MARA_S
        et_return =  DATA(lt_return_mara)     " Table Type for BAPIRET2
    ).
    LOOP AT lt_vbap INTO DATA(ls_vbap_skip) .
      " mutiple plants/stores found, stop processing
      IF ls_vbap_skip-werks NE ls_vbap-werks.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        ls_return-number = '013'.
        ls_return-message_v1 = ls_vbap-vbeln.
        SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
        MESSAGE e013(rfm_st_pick_ord)
        INTO ls_return-message
        WITH ls_vbap-vbeln.
        APPEND ls_return TO et_return.
        RETURN.
      ENDIF.
      " found an item where the shipping point used is invalid
      IF lv_impl_active = abap_false AND ms_c_pp_store-vstel IS NOT INITIAL.
        IF ls_vbap_skip-vstel NE ms_c_pp_store-vstel.
          ls_return-id = 'RFM_ST_PICK_ORD'.
          ls_return-type = 'E'.
          ls_return-number = '014'.
          ls_return-message_v1 = ls_vbap-vbeln.
          SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
          ls_return-message_v2 = ms_c_pp_store-vstel.
          ls_return-message_v3 = ls_vbap-werks.
          MESSAGE e014(rfm_st_pick_ord)
          INTO ls_return-message
          WITH ms_c_pp_store-vstel ls_vbap-werks.
          APPEND ls_return TO et_return.
          RETURN. " just a return statement will do, if a error message is required, we need to fill it inside the log
        ENDIF.
      ENDIF.

*      now we check for the type of the article, if it is not a retail article, stop the processing.
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_vbap-matnr.
      IF ls_mara-attyp IS INITIAL.
        ls_return-id = 'RFM_ST_PICK_ORD'.
        ls_return-type = 'E'.
        ls_return-number = '013'.
        ls_return-message_v1  = ls_vbap-vbeln.
        SHIFT ls_return-message_v1 LEFT DELETING LEADING '0'.
        ls_return-message_v2  = ls_mara-matnr.
        MESSAGE e013(rfm_st_pick_ord)
        INTO ls_return-message
        WITH ms_c_pp_store-vstel ls_vbap-werks.
        APPEND ls_return TO et_return.
        RETURN.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING lt_vbap TO mt_vbap.

    " finally we can call the method to create store order and within that we call the picking request generation method.
    create_store_order( IMPORTING et_return = DATA(lt_return) ).
    IF lt_return IS NOT INITIAL.
      APPEND LINES OF lt_return TO et_return.
    ENDIF.


  ENDMETHOD.


  METHOD create_store_order.

    DATA: lt_store_order      TYPE rfm_st_pick_ord_t,
          ls_vbap             TYPE vbap,
          lv_time_stamp       TYPE timestamp,
          lv_pmnt_status      TYPE rfm_st_order_payment_status,
          lt_order_header     TYPE if_rfm_st_pick_ord_split_prod=>tt_order_header,
          lt_order_det        TYPE if_rfm_st_pick_ord_split_prod=>tt_order_det,
          ls_order_det        TYPE if_rfm_st_pick_ord_split_prod=>ty_order_det,
          ls_order_header     TYPE if_rfm_st_pick_ord_split_prod=>ty_order_header,
          lo_st_ord_split     TYPE REF TO if_rfm_st_pick_ord_split_prod,
          lo_rfm_split_mod    TYPE REF TO rfm_st_pick_req_split_mod,
          lt_return_split     TYPE tt_bapiret2,
          lt_vbap_badi        TYPE TABLE OF vbap,
          ls_vbak_badi        TYPE vbak,
          lt_vbep_badi        TYPE TABLE OF vbep,
          lt_st_pick_req_item TYPE rfm_st_pick_reqi_t,
          lt_st_pick_req      TYPE rfm_st_pick_req_t.
    " we check ms_vbak, this gives us the surity that this is from sales order

    IF ms_vbak IS NOT INITIAL.
      IF mt_vbap IS NOT INITIAL.
        READ TABLE mt_vbap INTO ls_vbap INDEX 1.
      ENDIF.
      prepare_store_order(
        IMPORTING
          et_store_order = lt_store_order                 " Table type for structure RTST_PP_ORDERS
          et_return = DATA(lt_return_prep_ord)
      ).
      IF lt_return_prep_ord IS NOT INITIAL.
        APPEND LINES OF lt_return_prep_ord TO et_return.
      ENDIF.
*      prepare inputs for picking request generation
            prepare_store_pkr_request(
              IMPORTING
                et_order_details = lt_order_det
                et_order_header  = lt_order_header
            ).

      TRY .
          GET BADI lo_rfm_split_mod. "BADI to call the custom split logic
          IF lo_rfm_split_mod IS BOUND .

            CALL BADI lo_rfm_split_mod->modify_split_st_pick_req
              EXPORTING
                order_header_in         = lt_order_header                 " Sales Document: Header Data
                order_item_in           = lt_order_det                 " Sales Document: Item Data
              CHANGING
                et_return               = lt_return_split
                it_st_pick_req_item_out = lt_st_pick_req_item
                it_st_pick_req_head_out = lt_st_pick_req
              .
*            CATCH cx_ble_runtime_error. " BLE: Runtime Error
*              EXPORTING
*                so_header_in            = ms_vbak                 " Sales Document: Header Data
*                so_item_in              = mt_vbap                 " Sales Document: Item Data
*                so_sched_in             = mt_vbep                 " Sales Document: Schedule Line Data
*              CHANGING
*                it_st_pick_req_head_out = lt_st_pick_req
*                it_st_pick_req_item_out = lt_st_pick_req_item
*                et_return               = lt_return_split.
          ELSE.
            " in case the custom logic is not implemented the default logic for splitting will be called which is based on weight and volume.
            " after the creation of store order we call the method to generate store picking requests
            IF lo_st_ord_split IS NOT BOUND.
              lo_st_ord_split = cl_rfm_st_pick_order_factory=>get( )->get_split_prod( ).
            ENDIF.


            lo_st_ord_split->split_products(
              EXPORTING
                it_order_header     = lt_order_header
                it_order_details    = lt_order_det
              IMPORTING
                et_st_pick_req      = lt_st_pick_req
                et_st_pick_req_item = lt_st_pick_req_item
                et_return           = lt_return_split
            ).
          ENDIF.
        CATCH cx_ble_runtime_error. " BLE: Runtime Error .

      ENDTRY.
    ENDIF.

    IF lt_return_split IS INITIAL.
      mo_st_pkr->create_st_pick_request(
        EXPORTING
          it_st_pick_head =  lt_st_pick_req                " Table type of structure RFM_ST_PICK_REQ
          it_st_pick_item =  lt_st_pick_req_item                " Table type of structure RFM_ST_PICK_REQI
        IMPORTING
          et_return       =   DATA(lt_return_create_pkr)               " Table Type for BAPIRET2
          et_st_pick_head =   DATA(lt_st_pick_req_cr)               " Table type of structure RFM_ST_PICK_REQ
          et_st_pick_item =   DATA(lt_st_pick_reqi_cr)               " Table type of structure RFM_ST_PICK_REQI
      ).
      ELSEIF lt_return_split IS NOT INITIAL.
          APPEND LINES OF lt_return_split to et_return.
    ENDIF.
*    CALL METHOD mo_st_pkr->create_st_pick_request
*      EXPORTING
*        iv_order_number  = ms_vbak-vbeln
*        it_order_details = lt_order_det
*        it_order_header  = lt_order_header
*      IMPORTING
*        et_return        = DATA(lt_return_create_pkr)
*        et_pp_request    = DATA(lt_pp_request)
*        et_pp_req_item   = DATA(lt_pp_req_item).
*
    IF lt_return_create_pkr IS INITIAL.
      mo_st_ord_util->create_db_objects(
        EXPORTING
          it_pp_orders   = lt_store_order
          it_pp_req      = lt_st_pick_req_cr
          it_pp_req_item = lt_st_pick_reqi_cr
          IMPORTING
            et_return = DATA(lt_return_db_create)
      ).
      IF lt_return_db_create IS NOT INITIAL.
        APPEND LINES OF lt_return_db_create TO et_return.
      ENDIF.
    ELSE.
*      APPEND LINES OF lt_return_split TO et_return.
      APPEND LINES OF lt_return_create_pkr TO et_return.
    ENDIF.
*    MODIFY rtst_pp_orders FROM TABLE lt_rtst_pp_orders.




  ENDMETHOD.


  METHOD delete_store_order.
    DATA: lt_st_pick_orders TYPE rfm_st_pick_ord_t.
* just read store pickup orders and call the utility method to delete all the store picking orders and subsequently store picking requsts as well
    IF iv_st_order IS NOT INITIAL.
      " first check if the order exists in the db
      mo_st_ord_util->read_st_pick_orders(
        EXPORTING
          iv_st_order =   iv_st_order               " Table type for structure RTST_PP_ORDERS
        IMPORTING
          et_return    = DATA(lt_return_read_single)                 " Table Type for BAPIRET2
          et_pp_orders = DATA(lt_ord_single)                  " Table type for structure RTST_PP_ORDERS
      ).
      IF lt_return_read_single IS INITIAL.
        APPEND LINES OF lt_ord_single TO lt_st_pick_orders.
      ELSE.
        APPEND LINES OF lt_return_read_single TO et_return.
      ENDIF.
    ENDIF.

    IF it_st_order IS NOT INITIAL.
      mo_st_ord_util->read_st_pick_orders(
        EXPORTING
          it_pp_orders =   it_st_order               " Table type for structure RTST_PP_ORDERS
        IMPORTING
          et_return    = DATA(lt_return_read_tab)                 " Table Type for BAPIRET2
          et_pp_orders = DATA(lt_ord_tab)                  " Table type for structure RTST_PP_ORDERS
      ).
      IF lt_return_read_tab IS INITIAL.
        APPEND LINES OF lt_ord_tab TO lt_st_pick_orders.
      ELSE.
        APPEND LINES OF lt_return_read_tab TO et_return.
      ENDIF.

    ENDIF.
    IF et_return IS INITIAL.
      IF lt_st_pick_orders IS NOT INITIAL.
        mo_st_ord_util->delete_db_objects( EXPORTING it_st_order = lt_st_pick_orders IMPORTING et_return = DATA(lt_return_delete) ).
        IF lt_return_delete IS NOT INITIAL  .
          APPEND LINES OF lt_return_delete TO et_return.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order~cancel_substitute.

    CALL FUNCTION 'RFM_ST_PICKORD_SUBSTCANCEL_RFC' IN BACKGROUND TASK
      EXPORTING
        iv_store                   = iv_store
        iv_sales_order             = iv_sales_order
        iv_requested_product       = iv_requested_product
        iv_requested_product_uom   = iv_requested_product_uom
        iv_substituted_product     = iv_substituted_product
        iv_substituted_product_uom = iv_substituted_product_uom
        iv_rejection_reason_code   = iv_rejection_reason_code
* IMPORTING
*       ET_MESSAGES                =
      .

  ENDMETHOD.


  METHOD if_rfm_st_pick_order~create_store_order.


* 1.Read VBAK table in the beginning to get the type of the order, if it is a sales order or not. For STO we need to call table EKKO
* 2.if correspoding VBAK is not initial, then we need to check the type of the order, for sales order the type should be 'C' i.e. an order.
* 3.After this we call the core method to create Store pickup order with ref of sales order.
    IF gv_action_from_api = abap_true.
      gv_action_from_api = abap_false.
      RETURN.
    ENDIF.
    DATA: lt_vbap TYPE TABLE OF vbap.
    DATA: ls_vbap TYPE vbak.
    set_objects( ).
    IF  cl_rfm_st_pick_order=>gv_call_update_task = abap_true.
      go_application_log->add_message(
                EXPORTING
                  iv_msgno = '042'
                  iv_msgty = 'I'
                  iv_msgid = 'RFM_ST_PICK_ORD'
                  iv_msgv1 = ref_ord_num
              ).
      IF 1 = 2. MESSAGE i042(rfm_st_pick_ord) INTO DATA(lv_dumy). ENDIF.
    ENDIF.


    IF is_c_pp_store IS NOT INITIAL.
      ms_c_pp_store = is_c_pp_store.
    ELSE.
      mo_st_ord_util->read_c_pick_ord(
        IMPORTING
          es_c_pp_store = DATA(ls_c_pp_store)                  " Customizing for Store pickup order
      ).
      ms_c_pp_store = ls_c_pp_store.
    ENDIF.
    IF is_fpltc IS NOT INITIAL.
      mv_payment_status = is_fpltc-aunum.
    ENDIF.
    check_order_type(
    EXPORTING
      ref_ord_num = ref_ord_num
      is_vbak     = is_vbak
    IMPORTING
      et_return   = DATA(lt_return_type)
      ).
    IF lt_return_type IS NOT INITIAL.

      LOOP AT lt_return_type INTO DATA(ls_return_type).
        APPEND ls_return_type TO et_return.
      ENDLOOP.
    ENDIF.
    " if ms_vbak is filled then we can be sure that this is a sales order
    IF ms_vbak IS NOT INITIAL.
      " do we need to implement exceptions here??????
      create_from_sd_order( EXPORTING it_vbap = it_vbap it_vbep = it_vbep IMPORTING et_return = DATA(lt_return_ord) ).
    ENDIF.

    IF lt_return_ord IS NOT INITIAL.
      LOOP AT lt_return_ord INTO DATA(ls_return_ord).
        APPEND ls_return_ord TO et_return.
      ENDLOOP.
    ENDIF.
*    IF cl_rfm_st_pick_order=>gv_call_update_task = abap_true.
*      LOOP AT et_return ASSIGNING FIELD-SYMBOL(<lfs_message>).
*        go_application_log->add_message(
*           EXPORTING
*             iv_msgno = <lfs_message>-number
*             iv_msgty = <lfs_message>-type
*             iv_msgid = <lfs_message>-id
*             iv_msgv1 = <lfs_message>-message_v1
*             iv_msgv2 = <lfs_message>-message_v2
*             iv_msgv3 = <lfs_message>-message_v3
*             iv_msgv4 = <lfs_message>-message_v4 ).
*      ENDLOOP.
*
*      """ go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_picking_req_api=>gc_appl_log_subobject ).
*      go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_pickup_ord_api=>gc_appl_log_subobject ).
*    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order~delete_store_order.
    set_objects( ).
    delete_store_order(
      EXPORTING
        iv_st_order = iv_st_order                 " Store pickup order
        it_st_order = it_st_order                 " Table type for structure RTST_PP_ORDERS
      IMPORTING
        et_return   = et_return                 " Table Type for BAPIRET2
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

      go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_pickup_ord_api=>gc_appl_log_subobject ).
    ENDIF.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order~process_pgi_invoice_of_stord.
    DATA: lt_return TYPE tt_bapiret2.
    gv_action_from_api = abap_true.
*   CALL FUNCTION 'RFM_STORE_HP_HANDOVERORD_RFC' IN BACKGROUND TASK
    CALL FUNCTION 'RFM_ST_PICKORD_HANDOVER_RFC' IN BACKGROUND TASK
      EXPORTING
        iv_storeorder = iv_storeorder
        iv_store      = iv_store.
*      IMPORTING
*        et_return     = lt_return.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order~reversal_of_stord_when_cancel.
    DATA: lt_return TYPE tt_bapiret2.
    gv_action_from_api = abap_true.
*   CALL FUNCTION 'RFM_STORE_HP_CANCELORDER_RFC' IN BACKGROUND TASK
    CALL FUNCTION 'RFM_ST_PICKORD_CANCEL_RFC' IN BACKGROUND TASK
      EXPORTING
        iv_store                 = iv_store
        iv_storeorder            = iv_storeorder
        iv_rejection_reason_code = iv_rejection_reason_code
        iv_comment               = iv_comment.
*      IMPORTING
*        et_return                = lt_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order~reversal_of_stord_when_rho.
    DATA: lt_return TYPE tt_bapiret2.
    gv_action_from_api = abap_true.
*   CALL FUNCTION 'RFM_STORE_HP_RESHANDOVRORD_RFC' IN BACKGROUND TASK
    CALL FUNCTION 'RFM_ST_PICKORD_RESHANDOVER_RFC' IN BACKGROUND TASK
      EXPORTING
        iv_storeorder = iv_storeorder
*       iv_rejection_reason_code = iv_rejection_reason_code
        iv_store      = iv_store.
*      IMPORTING
*        et_return                = lt_return.

  ENDMETHOD.


  METHOD if_rfm_st_pick_order~set_object.
    mo_st_pkr = io_store_pkr.
    mo_st_ord_util = io_st_order_util.
  ENDMETHOD.


  METHOD if_rfm_st_pick_order~update_store_order.

    set_objects( ).
    CALL METHOD me->update_store_order
      EXPORTING
        it_store_order = it_st_pick_order
      IMPORTING
        et_return      = et_return.


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

    go_application_log->save_msg_to_bal( iv_subobject = if_rfm_st_pickup_ord_api=>gc_appl_log_subobject ).

  ENDMETHOD.


  METHOD prepare_store_order.
    DATA: ls_vbap           TYPE vbap,
          lv_time_stamp     TYPE timestamp,
          lv_pmnt_status    TYPE rfm_st_order_payment_status,
          ls_rtst_pp_orders TYPE rfm_st_pick_ord,
          so_date           LIKE sy-datum,
          so_time           LIKE sy-uzeit,
          lt_vbep           TYPE vbep_t,
          ls_order_data     TYPE if_rfm_st_pick_ord_subpref_mod=>ty_order,
          lo_rfm_sub_pref   TYPE REF TO rfm_st_pick_ord_subpref_mod.

    IF mt_vbap IS NOT INITIAL.
      READ TABLE mt_vbap INTO ls_vbap INDEX 1.
    ENDIF.
    IF mt_vbep IS NOT INITIAL.
      LOOP AT mt_vbep INTO DATA(ls_vbep) WHERE bmeng IS NOT INITIAL .
        APPEND ls_vbep TO lt_vbep.
      ENDLOOP.
      SORT lt_vbep BY edatu ezeit DESCENDING.

      READ TABLE lt_vbep INTO DATA(ls_vbep_sorted) INDEX 1.
      so_date = ls_vbep_sorted-edatu.
      so_time = ls_vbep_sorted-ezeit.
      CONVERT DATE so_date TIME so_time
      INTO TIME STAMP lv_time_stamp  TIME ZONE  'UTC'.
    ENDIF.
    ls_rtst_pp_orders-mandt = sy-mandt.
    ls_rtst_pp_orders-store = ls_vbap-werks.

    ls_rtst_pp_orders-storepickuporder = ms_vbak-vbeln.
    ls_rtst_pp_orders-storeorderexpdhandoverdtetme  = lv_time_stamp.
    ls_rtst_pp_orders-storeorderpickingstatus = cl_rfm_st_pick_request=>mv_ordr_pick_created.
    ls_rtst_pp_orders-storeorderoverallstatus = cl_rfm_st_pick_request=>mv_ordr_open.
    IF mv_payment_status IS NOT INITIAL.
      lv_pmnt_status = cl_rfm_st_pick_order_utility=>mv_ordr_paid.
    ELSE.
      mo_st_ord_util->get_payment_status(
        EXPORTING
          iv_vbeln  = ms_vbak-vbeln     " Sales Document
        IMPORTING
          ev_status = lv_pmnt_status   " Pickup order payment status
          et_return = DATA(lt_return_pmnt)" Return Parameter
      ).
      IF lt_return_pmnt IS NOT INITIAL.
        APPEND LINES OF lt_return_pmnt TO et_return.
      ENDIF.
    ENDIF.
    ls_rtst_pp_orders-storeorderpaymentstatus = lv_pmnt_status.

    GET BADI lo_rfm_sub_pref.

    TRY.
        IF lo_rfm_sub_pref  IS BOUND.
          CALL BADI lo_rfm_sub_pref->modify_substn_preference
            EXPORTING
              order_in                  = ms_vbak
              store                     = ls_rtst_pp_orders-store
            CHANGING
              is_substitution_preferred = ls_rtst_pp_orders-storepickuporderissubstnenbld.
        ENDIF.
      CATCH cx_ble_runtime_error INTO DATA(lx_exc).
    ENDTRY.

    APPEND ls_rtst_pp_orders TO et_store_order.
  ENDMETHOD.


  METHOD prepare_store_pkr_request.
    DATA: lt_order_header TYPE if_rfm_st_pick_ord_split_prod=>tt_order_header,
          lt_order_det    TYPE if_rfm_st_pick_ord_split_prod=>tt_order_det,
          ls_order_det    TYPE if_rfm_st_pick_ord_split_prod=>ty_order_det,
          ls_order_header TYPE if_rfm_st_pick_ord_split_prod=>ty_order_header,
          lt_vbep         TYPE vbep_t,
          lo_rfm_prd_seq  TYPE REF TO rfm_st_pick_ord_prd_seq_mod,
          lt_products     TYPE if_rfm_st_pick_ord_prd_seq_mod=>tt_matnr,
          lv_sequence     TYPE rfm_st_seq_num,
          lt_sequence     TYPE if_rfm_st_pick_ord_prd_seq_mod=>tt_sequence.

    ls_order_header-order_number = ms_vbak-vbeln.

    LOOP AT mt_vbep INTO DATA(ls_vbep) WHERE bmeng IS NOT INITIAL .
      APPEND ls_vbep TO lt_vbep.
    ENDLOOP.
    SORT lt_vbep BY edatu ezeit DESCENDING.

    READ TABLE lt_vbep INTO DATA(ls_vbep_sorted) INDEX 1.
    ls_order_header-delivery_date = ls_vbep_sorted-edatu.
    ls_order_header-delivery_time = ls_vbep_sorted-ezeit.
    ls_order_header-storepickupordertype = '01'.



    LOOP AT mt_vbap INTO DATA(ls_vbap).
      ls_order_det-item_num = ls_vbap-posnr.
      ls_order_det-product = ls_vbap-matnr.
      ls_order_det-store = ls_vbap-werks.
      ls_order_det-order_qty_in_sales_unit = ls_vbap-kwmeng.
      ls_order_det-sales_unit = ls_vbap-vrkme.
      ls_order_det-gross_weight = ls_vbap-brgew.
      ls_order_det-gross_weight_unit = ls_vbap-gewei.
      ls_order_det-gross_volume = ls_vbap-volum.
      ls_order_det-gross_volume_unit = ls_vbap-voleh.
      APPEND ls_order_det TO lt_order_det.
      CLEAR ls_order_det.
    ENDLOOP.

*            get picking sequence for the products

    GET BADI lo_rfm_prd_seq.
    TRY.
        IF lo_rfm_prd_seq  IS BOUND.
          CALL BADI lo_rfm_prd_seq->modify_product_sequence
            EXPORTING
              products_in          = lt_products
            CHANGING
              product_sequence_out = lt_sequence.
        ENDIF.
      CATCH cx_ble_runtime_error INTO DATA(lx_exc).
    ENDTRY.

    IF lt_sequence IS NOT INITIAL.
      LOOP AT lt_order_det ASSIGNING FIELD-SYMBOL(<fs_order_details>).
        READ TABLE lt_sequence INTO DATA(ls_sequence) WITH KEY product = <fs_order_details>-product.
        IF sy-subrc IS INITIAL.
          IF ls_sequence-sequence IS NOT INITIAL.
            <fs_order_details>-sequence = ls_sequence-sequence.
          ELSE.
            <fs_order_details>-sequence = '999999'.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT lt_order_det BY sequence ASCENDING.  " picking sequence integration

    APPEND LINES OF lt_order_det TO et_order_details.
    APPEND ls_order_header TO et_order_header.

  ENDMETHOD.


  METHOD set_objects.
    if_rfm_st_pick_order~set_object(
    io_store_pkr = cl_rfm_st_pick_request_factory=>get( )->get_st_pkr( )
    io_st_order_util = cl_rfm_st_pick_order_factory=>get( )->get_utility_obj( )
).
  ENDMETHOD.


  METHOD update_store_order.

    DATA : lt_rtst_pp_orders   TYPE rfm_st_pick_ord_t,
           ls_rtst_pp_orders   TYPE rfm_st_pick_ord_t,
           ls_store_order      TYPE rfm_st_pick_ord,
           lv_dummy            TYPE bapi_msg,
           lv_storepickuporder TYPE rfm_st_pickup_order,
           ls_return           TYPE bapiret2,
           lt_pp_orders        TYPE rfm_st_pick_ord_t.
    FIELD-SYMBOLS: <fs_rtst_pp_orders> TYPE  rfm_st_pick_ord.

*    mo_st_ord_util = cl_rfm_st_ord_factory=>get( )->get_utility_obj( ).

    IF it_store_order IS NOT INITIAL.

*  move this query to a method created only for select statements
*      SELECT * FROM rtst_pp_orders FOR ALL ENTRIES IN @it_store_order
*                                                   WHERE storepickuporder = @it_store_order-storepickuporder
*                                                   INTO TABLE @lt_rtst_pp_orders.

      CALL METHOD mo_st_ord_util->read_st_pick_orders
        EXPORTING
          it_pp_orders = it_store_order
        IMPORTING
          et_return    = DATA(lt_return_read)
          et_pp_orders = lt_rtst_pp_orders.


*      IF sy-subrc IS INITIAL.
      IF lt_return_read IS INITIAL.


*        check for all the statuses and other fileds to be updated one by one here
        LOOP AT lt_rtst_pp_orders ASSIGNING  <fs_rtst_pp_orders>.
          READ TABLE it_store_order INTO ls_store_order WITH KEY storepickuporder = <fs_rtst_pp_orders>-storepickuporder.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = <fs_rtst_pp_orders>-storepickuporder
              IMPORTING
                output = lv_storepickuporder.

            IF ls_store_order-storeorderpickingstatus IS NOT INITIAL. " update picking status of the store order
              CASE <fs_rtst_pp_orders>-storeorderpickingstatus.
                WHEN cl_rfm_st_pick_order_utility=>gv_pickingstatus_handedover. "05 , already handed over
                  CLEAR ls_return.
                  ls_return-id = 'RFM_ST_PICK_ORD'.
                  ls_return-type = 'I'.
                  ls_return-number = '027'.
                  ls_return-message_v1 = lv_storepickuporder.
                  IF 1 = 2. MESSAGE e027(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
                  APPEND ls_return TO et_return.
                  CONTINUE.

                WHEN cl_rfm_st_pick_order_utility=>gv_pickingstatus_resvhandovr. "06 , already reservation handed over
                  CLEAR ls_return.
                  ls_return-id = 'RFM_ST_PICK_ORD'.
                  ls_return-type = 'I'.
                  ls_return-number = '027'.
                  ls_return-message_v1 = lv_storepickuporder.
                  IF 1 = 2. MESSAGE e027(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
                  APPEND ls_return TO et_return.
                  CONTINUE.

                WHEN cl_rfm_st_pick_order_utility=>gv_pickingstatus_cancelorder. "07 , already cancelled
                  CLEAR ls_return.
                  ls_return-id = 'RFM_ST_PICK_ORD'.
                  ls_return-type = 'I'.
                  ls_return-number = '029'.
                  ls_return-message_v1 = lv_storepickuporder.
                  IF 1 = 2. MESSAGE e029(rfm_st_pick_ord) INTO lv_dummy. ENDIF.
                  APPEND ls_return TO et_return.
                  CONTINUE.
                WHEN cl_rfm_st_pick_request=>mv_ordr_pick_inprcss. "02 , already in process
                  IF ls_store_order-storeorderpickingstatus NE <fs_rtst_pp_orders>-storeorderpickingstatus.
                    <fs_rtst_pp_orders>-storeorderpickingstatus =  ls_store_order-storeorderpickingstatus.
                  ELSE.
                    CONTINUE.
                  ENDIF.

                WHEN OTHERS.
                  <fs_rtst_pp_orders>-storeorderpickingstatus =  ls_store_order-storeorderpickingstatus.
              ENDCASE.
            ENDIF.

            IF ls_store_order-storeorderoverallstatus IS NOT INITIAL. " update overall status of the store order
              <fs_rtst_pp_orders>-storeorderoverallstatus =  ls_store_order-storeorderoverallstatus.

*              check if order status is 'R' i.e. order is rejected. and then update the rejection reason
              IF ls_store_order-storeorderoverallstatus = 'R'.

                IF ls_store_order-storeorderrejectionreason IS  NOT INITIAL.
                  <fs_rtst_pp_orders>-storeorderrejectionreason = ls_store_order-storeorderrejectionreason.
                ENDIF.
              ENDIF.

            ENDIF.
            IF ls_store_order-nmbroffullypickeditems IS NOT INITIAL. " update number of fully picked items
              <fs_rtst_pp_orders>-nmbroffullypickeditems =  ls_store_order-nmbroffullypickeditems.
            ENDIF.
            IF ls_store_order-nmbrofunavailableitems IS NOT INITIAL. " update number of unavailable items
              <fs_rtst_pp_orders>-nmbrofunavailableitems =  ls_store_order-nmbrofunavailableitems.
            ENDIF.
            IF ls_store_order-nmbrofpartiallypickeditems IS NOT INITIAL. " update number of partially picked items
              <fs_rtst_pp_orders>-nmbrofpartiallypickeditems =  ls_store_order-nmbrofpartiallypickeditems.
            ENDIF.
          ENDIF.
          APPEND <fs_rtst_pp_orders> TO lt_pp_orders.
        ENDLOOP.

        CALL METHOD mo_st_ord_util->update_st_pick_orders
          EXPORTING
            it_pp_orders = lt_pp_orders
          IMPORTING
            et_return    = DATA(lt_return_upd).

        IF lt_return_upd IS NOT INITIAL.
          et_return = lt_return_upd.
*          READ TABLE lt_return_upd INTO DATA(ls_return_upd) WITH KEY type = 'S'.
*          IF sy-subrc IS INITIAL.
*            APPEND ls_return_upd TO et_return.
*          ENDIF.

        ENDIF.
      ELSE.
        et_return = lt_return_read.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.