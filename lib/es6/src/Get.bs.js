// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as MomentRe from "bs-moment/lib/es6/src/MomentRe.js";
import * as Belt_Array from "bs-platform/lib/es6/belt_Array.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Caml_splice_call from "bs-platform/lib/es6/caml_splice_call.js";
import * as Date$SqlitedbSlotsChallenge from "./Date.bs.js";
import * as Sqlite3$SqlitedbSlotsChallenge from "./Sqlite3.bs.js";

function dayToJs(param) {
  return param;
}

function is(day, date) {
  return date.getDay() === day;
}

function isSaturday(date) {
  return is(/* Saturday */6, date);
}

function isWithinInterval(interval, date) {
  var ts = Date$SqlitedbSlotsChallenge.copy(date).getTime();
  if (ts >= Date$SqlitedbSlotsChallenge.copy(interval.start).getTime()) {
    return ts < Date$SqlitedbSlotsChallenge.copy(interval.end_).getTime();
  } else {
    return false;
  }
}

function makeInterval(start, end_) {
  return {
          start: start,
          end_: end_
        };
}

function decodeEvent(r) {
  return {
          kind: r.kind,
          starts: new Date(r.starts_at),
          ends: new Date(r.ends_at),
          weekly_recurring: r.weekly_recurring
        };
}

function makeYMD(date) {
  return new Date(date.getFullYear(), date.getMonth(), date.getDate());
}

function makeResult(r, date) {
  var rangeStart = Date$SqlitedbSlotsChallenge.addDays(date, -1);
  var data = Belt_Array.map(r, decodeEvent);
  var openings = Belt_Array.keepMap(data, (function (x) {
          var match = x.kind === "opening";
          if (match) {
            return x;
          }
          
        }));
  var appointments = Belt_Array.keepMap(data, (function (x) {
          var match = x.kind === "appointment";
          if (match) {
            return x;
          }
          
        }));
  var appointment = Caml_array.caml_array_get(appointments, 0);
  var createAvailabilitiesArray = function (date) {
    return $$Array.init(7, (function (index) {
                  var __x = index + 1 | 0;
                  var date$1 = Date$SqlitedbSlotsChallenge.addDays(date, __x);
                  return {
                          date: date$1,
                          slots: []
                        };
                }));
  };
  var availabilities = createAvailabilitiesArray(rangeStart);
  var weekRangeStart = Caml_array.caml_array_get(data, 0).starts;
  var weekRangeEnd = Date$SqlitedbSlotsChallenge.addDays(weekRangeStart, 7);
  var weekInterval = {
    start: weekRangeStart,
    end_: weekRangeEnd
  };
  var generateApptSlots = function (param) {
    var formattedApptSlots = [];
    var starts = appointment.starts;
    var slotInterval_end_ = appointment.ends;
    var slotInterval = {
      start: starts,
      end_: slotInterval_end_
    };
    var slot = function (date) {
      if (isWithinInterval(slotInterval, date)) {
        var hours = date.toString();
        var formattedSlot = MomentRe.moment(undefined, hours).format("h:mm");
        formattedApptSlots.push(formattedSlot);
        var __x = Date$SqlitedbSlotsChallenge.copy(date);
        slot(Date$SqlitedbSlotsChallenge.addMinutes(__x, 30));
        return /* () */0;
      } else {
        return /* () */0;
      }
    };
    slot(starts);
    return formattedApptSlots;
  };
  var generateSlots = function (eventToCheck) {
    var formattedSlots = [];
    var starts = eventToCheck.starts;
    var slotInterval_end_ = eventToCheck.ends;
    var slotInterval = {
      start: starts,
      end_: slotInterval_end_
    };
    var slot = function (dateToCheck) {
      if (isWithinInterval(slotInterval, dateToCheck)) {
        var hours = dateToCheck.toString();
        var formattedSlot = MomentRe.moment(undefined, hours).format("h:mm");
        formattedSlots.push(formattedSlot);
        var __x = Date$SqlitedbSlotsChallenge.copy(dateToCheck);
        slot(Date$SqlitedbSlotsChallenge.addMinutes(__x, 30));
        return /* () */0;
      } else {
        return /* () */0;
      }
    };
    slot(starts);
    return formattedSlots;
  };
  var makeSlotTime = function (date, hours, minutes) {
    return new Date(date.getFullYear(), date.getMonth(), date.getDate(), hours, minutes);
  };
  Belt_Array.map(availabilities, (function (a) {
          var isSaturday = is(/* Saturday */6, a.date);
          var isInInterval = isWithinInterval(weekInterval, a.date);
          var slotStartHours = Caml_array.caml_array_get(openings, 0).starts.getHours();
          var slotStartMinutes = Caml_array.caml_array_get(openings, 0).starts.getMinutes();
          var slotEndsHours = Caml_array.caml_array_get(openings, 0).ends.getHours();
          var slotEndsMinutes = Caml_array.caml_array_get(openings, 0).ends.getMinutes();
          var starts = makeSlotTime(a.date, slotStartHours, slotStartMinutes);
          var ends = makeSlotTime(a.date, slotEndsHours, slotEndsMinutes);
          var init = Caml_array.caml_array_get(openings, 0);
          var currentEvent_kind = init.kind;
          var currentEvent_weekly_recurring = init.weekly_recurring;
          var currentEvent = {
            kind: currentEvent_kind,
            starts: starts,
            ends: ends,
            weekly_recurring: currentEvent_weekly_recurring
          };
          var newSlots = generateSlots(currentEvent);
          var hasAppointments = MomentRe.moment(undefined, makeYMD(a.date).toString()).isSame(MomentRe.moment(undefined, makeYMD(appointment.starts).toString()));
          if (isSaturday || !isInInterval) {
            return 0;
          } else if (hasAppointments) {
            return Caml_splice_call.spliceObjApply(a.slots, "push", [newSlots]);
          } else {
            var badSlots = generateApptSlots(/* () */0);
            var newSlotsFiltered = newSlots.filter((function (x) {
                    return !$$Array.mem(x, badSlots);
                  }));
            return Caml_splice_call.spliceObjApply(a.slots, "push", [newSlotsFiltered]);
          }
        }));
  return availabilities;
}

var db = Sqlite3$SqlitedbSlotsChallenge.Database.make("db.sqlite", undefined, undefined, undefined, true, /* () */0);

function getAvailabilities(date) {
  return makeResult(db.prepare("select * from `events`").all(), date);
}

export {
  dayToJs ,
  is ,
  isSaturday ,
  isWithinInterval ,
  makeInterval ,
  decodeEvent ,
  makeYMD ,
  makeResult ,
  db ,
  getAvailabilities ,
  
}
/* db Not a pure module */
