type event_token =
  | Down of (int * int)
  | Up of (int * int)
  | Move of (int * int)
  | Leave of (int * int)
  | Enter of (int * int)

type handler_state =
  | Nothing
  | Pan of (int * int) (* coordinates when pressing mouse *)
  | Drag of (int * int) (* not sure *)

exception UnExpectedEvent of string

type event_parser = handler_state -> event_token -> handler_state

let pan_handler cb s token = match s with
  | Nothing -> begin
      match token with
      | Down (x, y) -> Pan (x, y)
      | _ -> Nothing
    end
  | Pan (x, y) -> begin
      match token with
      | Down _ -> raise (UnExpectedEvent "Down in Pan")
      | Enter _ -> Nothing
      | Up (nx, ny) -> cb (x,y) (nx, ny) true; Nothing
      | Move (nx, ny) -> cb (x,y) (nx, ny) false; Pan (nx, ny)
      | Leave (nx, ny) -> cb (x,y) (nx, ny) true; Pan (nx, ny)
    end
  | _ -> Nothing
