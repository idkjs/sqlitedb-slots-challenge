// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var MomentRe = require("bs-moment/src/MomentRe.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_splice_call = require("bs-platform/lib/js/caml_splice_call.js");
var Date$DoctolibKnex = require("./Date.bs.js");
var Sqlite3$DoctolibKnex = require("./Sqlite3.bs.js");

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
  var ts = Date$DoctolibKnex.copy(date).getTime();
  if (ts >= Date$DoctolibKnex.copy(interval.start).getTime()) {
    return ts < Date$DoctolibKnex.copy(interval.end_).getTime();
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
  console.log(r);
  var event_kind = r.kind;
  var event_starts = new Date(r.starts_at);
  var event_ends = new Date(r.ends_at);
  var event_weekly_recurring = r.weekly_recurring;
  var $$event = {
    kind: event_kind,
    starts: event_starts,
    ends: event_ends,
    weekly_recurring: event_weekly_recurring
  };
  console.log("event", $$event);
  return $$event;
}

function makeYMD(date) {
  return new Date(date.getFullYear(), date.getMonth(), date.getDate());
}

function makeResult(r, date) {
  var rangeStart = Date$DoctolibKnex.addDays(date, -1);
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
                  var date$1 = Date$DoctolibKnex.addDays(date, __x);
                  return {
                          date: date$1,
                          slots: []
                        };
                }));
  };
  var availabilities = createAvailabilitiesArray(rangeStart);
  var weekRangeStart = Caml_array.caml_array_get(data, 0).starts;
  var weekRangeEnd = Date$DoctolibKnex.addDays(weekRangeStart, 7);
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
        var __x = Date$DoctolibKnex.copy(date);
        slot(Date$DoctolibKnex.addMinutes(__x, 30));
        return /* () */0;
      } else {
        return /* () */0;
      }
    };
    slot(starts);
    console.log("formattedApptSlots", formattedApptSlots);
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
        var __x = Date$DoctolibKnex.copy(dateToCheck);
        slot(Date$DoctolibKnex.addMinutes(__x, 30));
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
            console.log("hasAppointments_false_test", hasAppointments);
            return Caml_splice_call.spliceObjApply(a.slots, "push", [newSlots]);
          } else {
            console.log("hasAppointments_true_test", true);
            var badSlots = generateApptSlots(/* () */0);
            console.log("badSlots", badSlots);
            console.log("newSlots_before", newSlots);
            var newSlotsFiltered = newSlots.filter((function (x) {
                    return !$$Array.mem(x, badSlots);
                  }));
            console.log("newSlotsFiltered_after", newSlotsFiltered);
            return Caml_splice_call.spliceObjApply(a.slots, "push", [newSlotsFiltered]);
          }
        }));
  return availabilities;
}

var db = Sqlite3$DoctolibKnex.Database.make("db.sqlite", undefined, undefined, (function (prim) {
        console.log(prim);
        return /* () */0;
      }), true, /* () */0);

function getAvailabilities(date) {
  console.log("getdate", date);
  console.log("getdate", date.getUTCDate());
  var data = db.prepare("select * from `events`").all();
  console.log("data", data);
  var availabilities = makeResult(data, date);
  console.log("get_availabilities_result", availabilities);
  return availabilities;
}

var date = new Date("2014-08-10");

getAvailabilities(date);

exports.dayToJs = dayToJs;
exports.is = is;
exports.isSaturday = isSaturday;
exports.isWithinInterval = isWithinInterval;
exports.makeInterval = makeInterval;
exports.decodeEvent = decodeEvent;
exports.makeYMD = makeYMD;
exports.makeResult = makeResult;
exports.db = db;
exports.getAvailabilities = getAvailabilities;
exports.date = date;
/* db Not a pure module */
