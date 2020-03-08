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
  slots: array(MomentRe.Moment.t),
};
type availabilities = array(availability);
// type availabilityStrings = {
//   date,
//   slots: array(string),
// };

// type result = array(availabilityStrings);

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

let makeYMD = date => {
  Js.Date.(
    makeWithYMD(
      ~year=date->getFullYear,
      ~month=date->getMonth,
      ~date=date->getDate,
      (),
    )
  );
};

let makeResult = (r, date): availabilities => {
  // need to get availabilites for the 7 days in the range
  let rangeStartDate = date;
  // let rangeEndDate = rangeStartDate |> Date.addDays(_, 7);
  let rangeStart = rangeStartDate |> Date.addDays(_, -1);

  let data = Belt.Array.map(r, decodeEvent);
  Js.log2("data", data);
  let appointments = data[1];

  let apptStart =
    appointments.starts |> Js.Date.toString |> MomentRe.moment(~format=[||]);
  let aDate = date => date |> Js.Date.toString |> MomentRe.momentDefaultFormat;

  let hasAppointments =
    MomentRe.Moment.isSameWithGranularity(apptStart, aDate(date), `day);

  // creates a fixed array with 7 slots expected by test
  let arrayInitNextWeek: Js.Date.t => availabilities =
    date =>
      Array.init(7, index =>
        index->(+)(1)->Date.addDays(date, _)->(date => {date, slots: [||]})
      );

  let availabilities = arrayInitNextWeek(rangeStart);

  let {starts, weekly_recurring} = data[0];
  let weekRangeEnd = starts |> Date.addDays(_, 7);
  let weekInterval = makeInterval(makeYMD(starts), weekRangeEnd);

  let generateApptSlots = (~event) => {
    let formattedApptSlots = [||];
    let {starts, ends, _} = event;
    let slotInterval = makeInterval(starts, ends);

    let initDate = starts;
    let rec slot = date => {
      // Js.log2("rec.slot.date2", date);
      switch (isWithinInterval(slotInterval, date)) {
      | false => ()
      | true =>
        switch (hasAppointments) {
        | false => Js.log("hasAppointments_rec_false")
        | true => Js.log("hasAppointments_rec_true")
        };
        let starts = date |> Date.addMinutes(_, 30);
        let hours = starts |> Js.Date.toString;
        let formattedSlot = MomentRe.(moment(hours));

        Js.Array.push(formattedSlot, formattedApptSlots) |> ignore;

        slot(date |> Date.copy |> Date.addMinutes(_, 30)) |> ignore;
      };
    };

    slot(initDate);

    formattedApptSlots;
  };
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
        let formattedSlot = MomentRe.(moment(hours));

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
    open MomentRe;
    let newSlots: array(MomentRe.Moment.t) = generateSlots(~event=data[0]);
    let newApptSlots: array(MomentRe.Moment.t) =
      generateApptSlots(~event=data[1]);
    // Js.log2("newApptSlots",newApptSlots);

    availabilities->Belt.Array.map(a => {
      let isSaturday = isSaturday(a.date);
      let isInInterval = isWithinInterval(weekInterval, a.date);
      let apptInterval = makeInterval(appointments.starts, appointments.ends);
      let apptStart =
        appointments.starts |> Js.Date.toString |> momentDefaultFormat;
      let aDate = a.date |> Js.Date.toString |> momentDefaultFormat;

      let hasAppointments =
        Moment.isSameWithGranularity(apptStart, aDate, `day);
      // Js.log2("appointments", appointments);
      // Js.log2("hasAppointments", hasAppointments);
      // Js.log2("apptStart", appointments.starts);
      // Js.log2("momentWithDate(a.date)", a.date);
      let slotsToDate = Belt.Array.map(newSlots, s => s |> Moment.toDate);
      Js.log2("slotsToDate", slotsToDate);
      let handleSlotInterval =
        slotsToDate->Belt.Array.map(_, s =>
                       isWithinInterval(apptInterval, s)
                     );
      Js.log2("handleSlotInterval", handleSlotInterval);

      let showSlotsMoment =
        Belt.Array.map(newSlots, s => s |> Moment.defaultFormat);
      // Js.log2("showSlotsMoment", showSlotsMoment);
      let showApptSlotsMoment =
        Belt.Array.map(newApptSlots, s => s |> Moment.defaultFormat);
      // Js.log2("showApptSlotsMoment", showApptSlotsMoment);

      Js.log2("apptInterval", apptInterval);
      // Js.log2("isValid",showSlotsMoment[2]|>Js.Date.fromString|>Js.Date.)
      let checkAppts = slot =>
        Belt.Array.map(newApptSlots, nap =>
          if (nap !== slot) {
            Some(slot);
          } else {
            None;
          }
        );
      let checkAppts = slot =>
        Belt.Array.keepMap(newApptSlots, x =>
          if (x !== slot) {
            Some(x);
          } else {
            None;
          }
        );
      let test2 = newSlots |> Belt.Array.map(_, slot => checkAppts(slot));
      Js.log2("test2", test2[0]);

      Js.log(
        Belt.Array.cmp(newSlots, newApptSlots, (a, b) => compare(a, b)),
      );
      switch (hasAppointments) {
      | true =>
        Js.log2(
          "hasAppointments",
          Js.Array.filter(x => !Array.mem(x, newApptSlots), newSlots),
        )
      | _ => ()
      };
      switch (isSaturday, isInInterval) {
      | (false, true) => Js.Array.pushMany(newSlots, a.slots)
      | (_, _) => 0
      };
    })
    |> ignore;
  };

  availabilities;
};
open Sqlite3;

let db =
  Database.make(~path="db.sqlite", ~verbose=Js.log, ~fileMustExist=true, ());
[@genType]
let getAvailabilities = date => {
  let makeResultWithDate = makeResult(_, date);
  Js.log2("getdate", date);
  Js.log2("getdate", date |> Js.Date.getDay);
  let data = db->Database.prepare("select * from `events`")->Statement.all();
  let availabilities = makeResultWithDate(data);
  Js.log2("get_availabilities_result", availabilities);
  availabilities;
};
let date = "2014-08-10" |> Js.Date.fromString;
getAvailabilities(date);