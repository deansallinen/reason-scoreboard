type player = {
  id: int,
  score: int,
  name: string,
};

/* State declaration */
type state = {
  players: list(player),
  winner: string,
};

/* Action declaration */
type action =
  | AddPlayer(string)
  | NewGame
  | EndGame;

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

let str = ReasonReact.string;

let setStorage = (title, score: int) =>
  Dom.Storage.(localStorage |> setItem(title, string_of_int(score)));

let getStorage = name => Dom.Storage.(localStorage |> getItem(name));
let clearStorage = () => Dom.Storage.(localStorage |> clear);
let getKey = index => Dom.Storage.(localStorage |> key(index));
let lenStorage = Dom.Storage.(localStorage |> length);

let lastId = ref(0);

let newPlayer = name => {
  lastId := lastId^ + 1;
  {id: lastId^, name, score: 0};
};

/* let getPlayers = () => {
     let players = [];

     for (i in 1 to lenStorage) {
       switch (getKey(i)) {
       | Some(string) => [newPlayer(string), ...players]
       /* | None =>  */
       };
     };
   }; */

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
        className="input is-large is-rounded"
        value={self.state}
        type_="text"
        placeholder="Add a player..."
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

module ScoreInput = {
  type state = int;
  type action =
    | Update(int)
    | Clear;
  let component = ReasonReact.reducerComponent("ScoreInput");
  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => 0,
    reducer: (action, _state) =>
      switch (action) {
      | Update(score) => ReasonReact.Update(score)
      | Clear => ReasonReact.Update(0)
      },
    render: self =>
      <input
        className="input is-large is-rounded"
        value={string_of_int(self.state)}
        type_="number"
        onChange={
          event =>
            self.send(
              Update(int_of_string(ReactEvent.Form.target(event)##value)),
            )
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
  type state = {
    scores: list(int),
    total: int,
  };
  type action =
    | Increase(int);
  let component = ReasonReact.reducerComponent("Player");
  let make = (~name, _children) => {
    ...component,
    initialState: () => {total: 0, scores: []},
    reducer: (action, state) =>
      switch (action) {
      | Increase(score) =>
        let total = state.total + score;
        setStorage(name, total);
        ReasonReact.Update({total, scores: [score, ...state.scores]});
      },
    render: self =>
      <div className="container">
        <div className="level is-mobile">
          <span className="level-left is-size-2"> {str(name)} </span>
          <div
            className="level-right"
            style={ReactDOMRe.Style.make(~maxWidth="6rem", ())}>
            <ScoreInput onSubmit={score => self.send(Increase(score))} />
          </div>
        </div>
        <div>
          {
            ReasonReact.array(
              Array.of_list(
                List.rev(self.state.scores)
                |> List.map(score =>
                     <span> {str(string_of_int(score) ++ " ")} </span>
                   ),
              ),
            )
          }
        </div>
        {
          str(
            "Turns: "
            ++ string_of_int(List.length(self.state.scores))
            ++ " Total: "
            ++ string_of_int(self.state.total),
          )
        }
      </div>,
  };
};

let make = _children => {
  ...component,
  initialState: () => {players: [], winner: ""},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | AddPlayer(name) =>
      setStorage(name, 0);
      ReasonReact.Update({
        ...state,
        players: [newPlayer(name), ...state.players],
      });
    | NewGame =>
      clearStorage();
      ReasonReact.Update({winner: "", players: []});
    | EndGame =>
      List.map(
        player => (player.name, getStorage(player.name)),
        state.players,
      )
      |> List.map(each => Js.log(each));
      ReasonReact.NoUpdate;
    },

  render: self =>
    <div className="section">
      <div className="container"> {str("Winner")} </div>
      <div className="container">
        <PlayerInput onSubmit={event => self.send(AddPlayer(event))} />
      </div>
      <div className="container">
        {
          ReasonReact.array(
            Array.of_list(
              List.rev(self.state.players)
              |> List.map(player =>
                   <Player
                     key={string_of_int(player.id)}
                     name={player.name}
                   />
                 ),
            ),
          )
        }
      </div>
      <button className="button" onClick={_event => self.send(NewGame)}>
        {str("New Game")}
      </button>
      <button
        className="button is-warning" onClick={_event => self.send(EndGame)}>
        {str("End Game")}
      </button>
    </div>,
};