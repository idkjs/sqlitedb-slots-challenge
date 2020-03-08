type kind = string;
type starts = Js.Date.t;
type ends = Js.Date.t;
type weekly_recurring = bool;
type date = Js.Date.t;
type event = {
  kind,
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
      kind: r##kind,
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
let formatDate = date =>
  date |> Js.Date.toString |> MomentRe.moment(~format=[||]);
open MomentRe;
let makeResult = (r, date): availabilities => {
  // need to get availabilites for the 7 days in the range
  let rangeStartDate = date;
  // let rangeEndDate = rangeStartDate |> Date.addDays(_, 7);
  let rangeStart = rangeStartDate |> Date.addDays(_, -1);

  let data = Belt.Array.map(r, decodeEvent);
  Js.log2("data", data);
  let appointments =
    Belt.Array.keepMap(data, d => Some(d.kind == "appointment"));
  let openings =
    Belt.Array.keepMap(data, x =>
      switch (x.kind == "opening") {
      | true => Some(x)
      | _ => None
      }
    );
  let appointments =
    Belt.Array.keepMap(data, x =>
      switch (x.kind == "appointment") {
      | true => Some(x)
      | _ => None
      }
    );
  let appointment = appointments[0];
  let appt_start = makeYMD(appointment.starts);
  let appt_start2 =
    makeYMD(appointment.starts)
    |> Js.Date.toString
    |> MomentRe.moment(~format=[|"YYYY-MM-DD"|]);
  // Js.log2("appt_start2", appt_start2);
  let appt_start3 = makeYMD(appointment.starts);

  Js.log2("appt_start3", appt_start3);
  let apptStart =
    appointment.starts |> Js.Date.toString |> MomentRe.moment(~format=[||]);
  let aDate = date => date |> MomentRe.momentWithDate;
  let aDate2 = date =>
    makeYMD(date)
    |> Js.Date.toString
    |> MomentRe.moment(~format=[|"YYYY-MM-DD"|]);
  let aDate3 = date => makeYMD(date) |> Js.Date.toString;

  let hasAppointments =
    MomentRe.Moment.isSameWithGranularity(
      MomentRe.momentWithDate(appt_start),
      aDate(date),
      `day,
    );
  let hasAppointments2 =
    MomentRe.Moment.isSameWithGranularity(appt_start2, aDate2(date), `day);
  let hasAppointments2 =
    Moment.isSame(
      moment(aDate3(date)),
      moment(appt_start3 |> Js.Date.toString),
    );
  // let hasAppointments =date =>
  //   MomentRe.Moment.isSameWithGranularity(apptStart, aDate(date), `day);

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
  let generateApptSlots = () => {
    let formattedApptSlots = [||];
    let {starts, ends, _} = appointment;
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

        Js.Array.push(formattedSlot, formattedApptSlots) |> ignore;

        slot(date |> Date.copy |> Date.addMinutes(_, 30)) |> ignore;
      };
    };

    slot(initDate);

    formattedApptSlots;
  };
  let generateSlots = (~eventToCheck) => {
    let formattedSlots = [||];
    let {starts, ends, _} = eventToCheck;
    // Js.log2("rec.slot.eventToCheck", eventToCheck);
    let slotInterval = makeInterval(starts, ends);
    // Js.log2("rec.slot.slotInterval", slotInterval);

    let initDate = starts;
    let rec slot = dateToCheck => {
      //  Js.log2("Slot2MomentIsSame",Moment.isSame(moment(aDate3(dateToCheck)), moment(appt_start3|>Js.Date.toString)))
      // Js.log2("rec.slot.dateToCheck", dateToCheck);
      let makeIsSameDateToCheck = Js.Date.toString(makeYMD(dateToCheck));

      switch (isWithinInterval(slotInterval, dateToCheck)) {
      | false => ()
      | true =>
        switch (
          Moment.isSame(
            moment(makeIsSameDateToCheck),
            moment(appt_start3 |> Js.Date.toString),
          )
        ) {
        | false =>
          let starts = dateToCheck |> Date.addMinutes(_, 30);
          let hours = starts |> Js.Date.toString;

          let formattedSlot =
            MomentRe.(moment(hours) |> Moment.format("h:mm"));

          Js.Array.push(formattedSlot, formattedSlots) |> ignore;

          slot(dateToCheck |> Date.copy |> Date.addMinutes(_, 30)) |> ignore;
        // | false => Js.log2("hasAppointments_rec_false", dateToCheck)
        | true =>
          Js.log3("hasAppointments_rec_true", dateToCheck, true);
          let formattedApptSlots = [||];
          Js.log2("formattedSlots_empty", formattedApptSlots);
          let starts = dateToCheck |> Date.addMinutes(_, 30);
          let hours = starts |> Js.Date.toString;
          let formattedSlot =
            MomentRe.(moment(hours) |> Moment.format("h:mm"));

          Js.Array.push(formattedSlot, formattedApptSlots) |> ignore;
          Js.log2("formattedSlot", formattedSlot);
          Js.log2("formattedApptSlots_before", formattedApptSlots);
          let badSlots = generateApptSlots();
          Js.Array.filter(x => !Array.mem(x, badSlots), formattedApptSlots)
          |> ignore;
          Js.log2("formattedApptSlots_filtered", formattedApptSlots);
          Js.log2("badSlots", badSlots);
          let filteredSlots =
            Js.Array.filter(
              x => !Array.mem(x, badSlots),
              formattedApptSlots,
            );
            Js.log2("filteredSlots", filteredSlots);
          Js.Array.concatMany([|filteredSlots|], formattedSlots) |> ignore;
          Js.log2("formattedSlots_filtered", formattedSlots);
          slot(dateToCheck |> Date.copy |> Date.addMinutes(_, 30)) |> ignore;
        }
      };
    };

    slot(initDate);

    formattedSlots;
  };
  // switch (weekly_recurring) {
  // | false => ()
  // | true =>
  //   Js.log(true);
  // let newSlots: array(string) = generateSlots(~event=data[0]);
  open Js.Date;
  let makeSlotTime = (date, hours, minutes) => {
    makeWithYMDHM(
      ~year=date->getFullYear,
      ~month=date->getMonth,
      ~date=date->getDate,
      ~hours,
      ~minutes,
      (),
    );
  };
  availabilities->Belt.Array.map(a => {
    let isSaturday = isSaturday(a.date);
    let isInInterval = isWithinInterval(weekInterval, a.date);
    let slotStartHours = openings[0].starts |> Js.Date.getHours;
    let slotStartMinutes = openings[0].starts |> Js.Date.getMinutes;
    let slotEndsHours = openings[0].ends |> Js.Date.getHours;
    let slotEndsMinutes = openings[0].ends |> Js.Date.getMinutes;
    let starts = makeSlotTime(a.date, slotStartHours, slotStartMinutes);
    let ends = makeSlotTime(a.date, slotEndsHours, slotEndsMinutes);
    Js.log2(
      "MomentIsSame",
      Moment.isSame(
        moment(makeYMD(a.date) |> toString),
        moment(appt_start3 |> toString),
      ),
    );
    Js.log2("currentDate", starts);
    // Js.log2("currentDate", currentDate|>Js.Date.(parseAsFloat))
    let currentEvent = {...openings[0], starts, ends};
    let newSlots: array(string) = generateSlots(~eventToCheck=currentEvent);
    Js.log2("appointment.starts", appointment.starts |> getTime);
    switch (isSaturday, isInInterval) {
    | (false, true) => Js.Array.pushMany(newSlots, a.slots)
    | (_, _) => 0
    };
  })
  |> ignore;
  // };

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