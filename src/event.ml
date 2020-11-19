type event_token =
  | Down of (int * int)
  | Up of (int * int)
  | Move of (int * int)
  | Leave of (int * int)
  | Enter of (int * int)

type drag_state =
  | Nothing
  | Drag of (int * int) (* not sure *)

exception UnExpectedEvent of string

type event_parser = drag_state -> event_token -> drag_state

let pan_handler cb s token = match s with
  | Nothing -> begin
      match token with
      | Down (x, y) -> Drag (x, y)
      | _ -> Nothing
    end
  | Drag (x, y) -> begin
      match token with
      | Down _ -> Nothing
      | Enter _ -> Nothing
      | Up (nx, ny) -> cb (x,y) (nx, ny) true; Nothing
      | Move (nx, ny) -> cb (x,y) (nx, ny) false; Drag (nx, ny)
      | Leave (nx, ny) -> cb (x,y) (nx, ny) true; Nothing
    end
