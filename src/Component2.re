type player = {
  id: int,
  score: int,
  name: string,
};

/* State declaration */
type state = {players: list(player)};

/* Action declaration */
type action =
  | AddPlayer(string);

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

let str = ReasonReact.string;

let setStorage = (title, score: int) =>
   Dom.Storage.(localStorage |> setItem(title, string_of_int(score)));

let lastId = ref(0);

let newPlayer = name => {
  lastId := lastId^ + 1;
  {id: lastId^, name, score: 0};
};

module PlayerInput = {
  type state = string;
  type action =
    | Update(string)
    | Clear;
  let component = ReasonReact.reducerComponent("PlayerInput");
  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => "",
    reducer: (action, _state) =>
      switch (action) {
      | Update(text) => ReasonReact.Update(text)
      | Clear => ReasonReact.Update("")
      },
    render: self =>
      <input
        value={self.state}
        type_="text"
        placeholder="Name of person"
        onChange={
          event => self.send(Update(ReactEvent.Form.target(event)##value))
        }
        onKeyDown={
          evt =>
            if (ReactEvent.Keyboard.key(evt) == "Enter") {
              onSubmit(self.state);
              self.send(Clear);
            }
        }
      />,
  };
};

module Player = {
  type state = int;
  type action =
    | Increase;
  let component = ReasonReact.reducerComponent("Player");
  let make = (~id, ~name, ~score, _children) => {
    ...component,
    initialState: () => 0,
    reducer: (action, state) =>
      switch (action) {
      | Increase => ReasonReact.Update(state + 1)
      },
    render: self =>
      <div>
        {str(name)}
        <button onClick={_event => self.send(Increase)}>
          {str(string_of_int(self.state))}
        </button>
      </div>,
  };
};

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = (~greeting, _children) => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {players: []},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | AddPlayer(name) => {
      setStorage(name, 0)
      ReasonReact.Update({players: [newPlayer(name), ...state.players]})
    }
    },

  render: self =>
    <div>
      <PlayerInput onSubmit={event => self.send(AddPlayer(event))} />
      <div>
        {
          ReasonReact.array(
            Array.of_list(
              List.map(
                player =>
                  <Player
                    key={string_of_int(player.id)}
                    id={player.id}
                    name={player.name}
                    score={player.score}
                  />,
                self.state.players,
              ),
            ),
          )
        }
      </div>
    </div>,
};