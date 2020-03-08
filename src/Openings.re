type kind = string;
type starts = Js.Date.t;
type ends = Js.Date.t;
type weekly_recurring = bool;
type date = Js.Date.t;
type event = {
  starts,
  ends,
  weekly_recurring,
};

type availability = {
  date,
  slots: array(string),
};
type availabilities = array(availability);

type interval = {
  start: Js.Date.t,
  end_: Js.Date.t,
};
type day =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday;
let dayToJs =
  fun
  | Sunday => 0
  | Monday => 1
  | Tuesday => 2
  | Wednesday => 3
  | Thursday => 4
  | Friday => 5
  | Saturday => 6;

let is = (day, date) =>
  date |> Js.Date.getDay === (day |> dayToJs |> float_of_int);

let isWeekend = date => date |> is(Saturday) || date |> is(Sunday);
let isSunday = date => date |> is(Sunday);
let isSaturday = date => date |> is(Saturday);
let isWithinInterval = (interval, date) => {
  let ts = date |> Date.copy |> Js.Date.getTime;
  ts >= (interval.start |> Date.copy |> Js.Date.getTime)
  && ts <= (interval.end_ |> Date.copy |> Js.Date.getTime);
};
let makeInterval = (start, end_) => {start, end_};
let decodeEvent: Js.t('a) => event =
  r => {
    Js.log(r);
    let event: event = {
      starts: r##starts_at |> Js.Date.fromFloat,
      ends: r##ends_at |> Js.Date.fromFloat,
      weekly_recurring: r##weekly_recurring,
    };
    Js.log2("event", event);
    event;
  };
// let availabilities: array(availability) = [||];
// Js.log2("availabilities_start", availabilities);

// let eventsArr: array(event) = [||];
let makeYMD = date => {
  Js.Date.makeWithYMD(
    ~year=date->Js.Date.getFullYear,
    ~month=date->Js.Date.getMonth,
    ~date=date->Js.Date.getDate,
    (),
  );
};
let makeResult = (r, date): availabilities => {
  // need to get availabilites for the 7 days in the range
  let rangeStartDate = date;
  let rangeStartDateIsWE = isWeekend(date);
  let rangeStartDateIsWE = isSunday(date);
  let rangeEndDate = rangeStartDate |> Date.addDays(_, 7);
  let rangeStart = rangeStartDate |> Date.addDays(_, -1);
  // Js.log2("rangeStart",rangeStart)
  Js.log2("rangeStartDateIsWE", rangeStartDateIsWE);
  Js.log2("rangeStartDateIsWE", rangeStartDate |> Js.Date.getDay);

  let opening = decodeEvent(r);
  // creates a fixed array with 7 slots expected by test
  let arrayInitNextWeek: Js.Date.t => availabilities =
    date =>
      Array.init(7, index =>
        index->(+)(1)->Date.addDays(date, _)->(date => {date, slots: [||]})
      );

  let availabilities = arrayInitNextWeek(rangeStart);
  let checkWeekend = availabilities->Belt.Array.map(a => isWeekend(a.date));
  Js.log2("checkWeekend", checkWeekend);
  let {starts, ends, weekly_recurring} = opening;
  let weekRangeEnd = starts |> Date.addDays(_, 7);
  let weekInterval = makeInterval(makeYMD(starts), weekRangeEnd);
  Js.log2("weekInterval_start", weekInterval.start);
  Js.log2("weekInterval_end", weekInterval.end_);
  Js.log2("weekInterval_end", weekInterval.end_);
  Js.log2("weekInterval", weekInterval);
  let generateSlots = (~event) => {
    let formattedSlots = [||];
    let {starts, ends, _} = event;
    let slotInterval = makeInterval(starts, ends);

    let initDate = starts;
    let rec slot = date => {
      // Js.log2("rec.slot.date2", date);
      switch (isWithinInterval(slotInterval, date)) {
      | false => ()
      | true =>
        let starts = date |> Date.addMinutes(_, 30);

        let hours = starts |> Js.Date.toString;
        let formattedSlot =
          MomentRe.(moment(hours) |> Moment.format("h:mm"));

        Js.Array.push(formattedSlot, formattedSlots) |> ignore;

        slot(date |> Date.copy |> Date.addMinutes(_, 30)) |> ignore;
      };
    };

    slot(initDate);

    formattedSlots;
  };
  switch (weekly_recurring) {
  | false => ()
  | true =>
    Js.log(true);

    let newSlots: array(string) = generateSlots(~event=opening);
    Js.log(newSlots);
    availabilities->Belt.Array.map(a => {
      let isSaturday = isSaturday(a.date);
      let isInInterval = isWithinInterval(weekInterval, a.date);
      Js.log2("isInInterval", isInInterval);
      switch (isSaturday, isInInterval) {
      | (false, true) => Js.Array.pushMany(newSlots, a.slots)
      | (_, _) => 0
      };
    })
    |> ignore;
  };
  // get number of days before first event by subtracting minDate from the maxDate

  availabilities;
};
// let knex = KnexDB.knex;
open Sqlite3;
type row = (int, string, int);
let db =
  Database.make(~path="db.sqlite", ~verbose=Js.log, ~fileMustExist=true, ());
[@genType]
// let getAvailabilities = (date): Js.Promise.t(availabilities) => {
let getAvailabilities = date => {
  let makeResultWithDate = makeResult(_, date);
  Js.log2("getdate", date);
  Js.log2("getdate", date |> Js.Date.getDay);
  let data2 = db->Database.prepare("SELECT * FROM events")->Statement.all();
  Js.log2("getdata2", data2);
  let data =
    db
    ->Database.prepare("SELECT * FROM events WHERE kind = 'opening'")
    ->Statement.get();
  Js.log2("getdata", data);
  // let data =
  //   db
  //   ->Database.prepare("select * from `events`")
  //   ->Statement.get();
  let availabilities = makeResultWithDate(data);
  Js.log2("get_availabilities_result", availabilities);
  // Js.Promise.resolve(availabilities);
  availabilities;
};
let date = "2014-08-10" |> Js.Date.fromString;
getAvailabilities(date);