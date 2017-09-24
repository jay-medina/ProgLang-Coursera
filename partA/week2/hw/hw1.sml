(* date is year, month, day *)

(* 1. *)
fun is_older (date1: (int*int*int), date2: (int*int*int)) =
  let
    val isYrLess = #1 date1 < #1 date2
    val isYrEqual = #1 date1 = #1 date2
    val isMonthLess = #2 date1 < #2 date2
    val isMonthEqual = #2 date1 = #2 date2
    val isDayLess = #3 date1 < #3 date2
  in
    isYrLess orelse 
    (isYrEqual andalso isMonthLess) orelse 
    (isYrEqual andalso isMonthEqual andalso isDayLess)
  end

(* 2. *)
fun number_in_month (dates: (int*int*int) list, month: int) =
  let
    fun month_counter (dates: (int*int*int) list) =
      if null dates
      then 0
      else 
        if #2 (hd dates) = month
        then 1 + month_counter (tl dates)
        else month_counter (tl dates)
  in
    month_counter(dates)
  end

(* 3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month ( dates: (int*int*int) list, month: int) =
  let
    fun month_adder (dates: (int*int*int) list) =
      if null dates
      then []
      else 
        if #2 (hd dates) = month
        then (hd dates) :: month_adder (tl dates)
        else month_adder (tl dates)
  in
    month_adder(dates)
  end

(* 5 *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6 *)
fun get_nth (strs: string list, n: int) =
 if n = 1
 then hd strs
 else get_nth (tl strs, n - 1)

(* 7 *)
fun date_to_string (date: (int*int*int)) =
  let
    val mths = ["January", "February", "March", "April", "May", "June", 
      "July", "August", "September", "October", "November", "December"]
    val month_as_string = get_nth(mths, #2 date)
    val day_as_string = Int.toString (#3 date)
    val yr_as_str = Int.toString (#1 date)
  in
    month_as_string ^ " " ^ day_as_string ^ ", " ^ yr_as_str
  end

(* 8 *)
fun number_before_reaching_sum (sum: int, nums: int list) =
  let
    fun loop(nums: int list, count: int, current_sum: int) =
      if null nums
      then count
      else if (hd nums) + current_sum >= sum
           then count - 1
           else loop(tl nums, count + 1, current_sum + (hd nums))
  in
    loop(nums, 1, 0)
  end

(* 9 *)
fun what_month (day_of_year: int) =
  let
    val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day_of_year, days_in_months) + 1
  end

(* 10 *)
fun month_range ( day1: int, day2: int) =
  if day1 > day2
  then []
  else 
    what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest (dates: (int*int*int) list) =
  if null dates
  then NONE
  else if null (tl dates)
  then SOME(hd dates)
  else
    let 
      fun findOldest(dates: (int*int*int) list, current: (int*int*int)) =
        if null (tl dates)
        then (if is_older(hd dates, current) then SOME(hd dates) else SOME(current))
        else if is_older(hd dates, current) 
        then findOldest(tl dates, hd dates)
        else findOldest(tl dates, current)
    in
      findOldest(tl dates, hd dates)
    end
