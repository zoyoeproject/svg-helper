type event_token =
  | Down of (int * int)
  | Up of (int * int)
  | Move of (int * int)

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
      | Up _ -> Nothing
      | Move _ -> Nothing
    end
  | Pan (x, y) -> begin
      match token with
      | Down _ -> raise (UnExpectedEvent "Down in Pan")
      | Up (nx, ny) -> cb (x,y) (nx, ny); Nothing
      | Move (nx, ny) -> cb (x,y) (nx, ny); Pan (nx, ny)
    end
  | _ -> Nothing




