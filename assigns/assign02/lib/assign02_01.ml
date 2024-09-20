type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index

let get_pos (board : board) (p_index : pos_index) : pos =
  let (row_01, row_02, row_03) = board in
  let (r_index, c_index) = p_index in
  let row = match r_index with
    | Top -> row_01
    | Middle -> row_02
    | Bottom -> row_03
  in
  let (c1, c2, c3) = row in
  match c_index with
    | Left -> c1
    | Middle -> c2
    | Right -> c3

let winner (board : board) : bool =
  let (row_01, row_02, row_03) = board in
  let (row_01_column_01, row_01_column_02, row_01_column_03) = row_01 in
  let (row_02_column_01, row_02_column_02, row_02_column_03) = row_02 in
  let (row_03_column_01, row_03_column_02, row_03_column_03) = row_03 in
  let win_check (p1, p2, p3) =
    match (p1, p2, p3) with
    | (Piece p1, Piece p2, Piece p3) when p1 = p2 && p2 = p3 -> true
    | _ -> false
  in
  win_check (row_01_column_01, row_01_column_02, row_01_column_03) || win_check (row_02_column_01, row_02_column_02, row_02_column_03) ||
  win_check (row_03_column_01, row_03_column_02, row_03_column_03) || win_check (row_01_column_01, row_02_column_01, row_03_column_01) || 
  win_check (row_01_column_02, row_02_column_02, row_03_column_02) || win_check (row_01_column_03, row_02_column_03, row_03_column_03) ||
  win_check (row_01_column_01, row_02_column_02, row_03_column_03) || win_check (row_01_column_03, row_02_column_02, row_03_column_01)  
