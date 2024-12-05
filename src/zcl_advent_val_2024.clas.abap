CLASS zcl_advent_val_2024 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS day1
      IMPORTING
        !input   TYPE string_table
      EXPORTING
        !output  TYPE int8
        !output2 TYPE int8.

    CLASS-METHODS day2
      IMPORTING
        !input   TYPE string_table
      EXPORTING
        !output  TYPE int8
        !output2 TYPE int8.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVENT_VAL_2024 IMPLEMENTATION.


  METHOD day1.
    TYPES: BEGIN OF ty_pair,
             left_value  TYPE i,
             right_value TYPE i,
           END OF ty_pair.

    DATA: lt_pairs            TYPE TABLE OF ty_pair,
          lt_sorted_left      TYPE TABLE OF i,
          lt_sorted_right     TYPE TABLE OF i,
          lv_total_distance   TYPE int8,
          lv_distance         TYPE int8,
          lv_similarity_score TYPE int8,
          lv_temp             TYPE int8,
          lv_count            TYPE i.

    " Transform the input string array into pairs of numbers
    LOOP AT input INTO DATA(lv_line).
      SPLIT lv_line AT '   ' INTO DATA(lv_left) DATA(lv_right).
      IF lv_left IS NOT INITIAL AND lv_right IS NOT INITIAL.
        APPEND VALUE #( left_value = lv_left right_value = lv_right ) TO lt_pairs.
      ENDIF.
    ENDLOOP.

    " Extract values into separate tables and sort them
    LOOP AT lt_pairs INTO DATA(ls_pair).
      APPEND ls_pair-left_value TO lt_sorted_left.
      APPEND ls_pair-right_value TO lt_sorted_right.
    ENDLOOP.

    SORT lt_sorted_left ASCENDING.
    SORT lt_sorted_right ASCENDING.

    " Calculate the total distance
    LOOP AT lt_sorted_left INTO DATA(lv_left_value).

      lv_temp = lv_left_value.

      " Use sy-tabix to determine the current index
      READ TABLE lt_sorted_right INTO DATA(lv_right_value) INDEX sy-tabix. " TODO: Read - do we need INDEX sy-tabix here?
      IF sy-subrc = 0.
        lv_distance = abs(  lv_temp - lv_right_value ).
        lv_total_distance = lv_total_distance + lv_distance.
      ENDIF.

      " TODO: REFACTOR
      " Calculate similarity score
      lv_count = 0.

      LOOP AT lt_sorted_right INTO DATA(lv_right_value2).
        IF lv_right_value2 = lv_left_value.
          ADD 1 TO lv_count.
        ENDIF.
      ENDLOOP.
      lv_similarity_score = lv_similarity_score + ( lv_left_value * lv_count ).

    ENDLOOP.

    " Set the output values
    output  = lv_total_distance.
    output2 = lv_similarity_score.

  ENDMETHOD.


  METHOD day2.
    DATA: lv_count_safe TYPE int8 VALUE 0,
          lt_levels     TYPE TABLE OF string,
          lv_increase   TYPE boolean,
          lv_decrease   TYPE boolean,
          lv_curr       TYPE i,
          lv_prev       TYPE i,
          lv_diff       TYPE i.

    LOOP AT input INTO DATA(lv_line).

      CLEAR: lt_levels, lv_increase, lv_decrease.

      " Split the line into individual levels
      SPLIT lv_line AT space INTO TABLE lt_levels.

      " Initialize the flags
      lv_increase = abap_true.
      lv_decrease = abap_true.

      LOOP AT lt_levels INTO DATA(lv_curr_str).
        " Convert string to integer
        lv_curr = lv_curr_str.

        " Skip the first level
        IF sy-tabix = 1.
          lv_prev = lv_curr.
          CONTINUE.
        ENDIF.

        " Calculate the difference
        lv_diff = lv_curr - lv_prev.

        " Check if the difference is outside the safe range
        IF abs( lv_diff ) < 1 OR abs( lv_diff ) > 3.
          lv_increase = abap_false.
          lv_decrease = abap_false.
          EXIT.
        ENDIF.

        " Update the flags
        IF lv_diff > 0.
          lv_decrease = abap_false.
        ELSEIF lv_diff < 0.
          lv_increase = abap_false.
        ENDIF.

        " Update the previous value
        lv_prev = lv_curr.
      ENDLOOP.

      " Check if the report is safe
      IF lv_increase = abap_true OR lv_decrease = abap_true.
        lv_count_safe = lv_count_safe + 1.
      ENDIF.

    ENDLOOP.

    " Set the output values
    output = lv_count_safe.

  ENDMETHOD.
ENDCLASS.
