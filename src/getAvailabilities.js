import moment from "moment";
import knex from "knexClient";

const OPENING = "opening";
const APPOINTMENT = "appointment";
const MINUTES = "minutes";
const DEFAULT_MINUTES_INCREMENT = 30;
const TIMESTAMP_FORMAT = "H:mm";
const DEFAULT_NUMBER_OF_DAYS = 10;
const YEAR_MONTH_DAY_FORMAT = "YYYY-MM-DD";
const DAY_FORMAT = "d";
const DAYS = "days";


/**
 * Get Events Data Order By Descending
 * Where weekly recurring is true
 * And the given date should less than end date
 * @param date
 * @returns {*}
 */
const getEventsFilterByDate = (date) => {
  return knex
      .select("kind", "starts_at", "ends_at", "weekly_recurring")
      .from("events")
      .where(function () {
        this.where("weekly_recurring", true).orWhere("ends_at", ">", +date);
      }).orderBy('kind', 'desc');
};


/**
 * Create New Set of available slot and date data
 * with the given date and total number of days
 * @param date
 * @param numberOfDays
 * @returns {Map<any, any>}
 */
const setAvailabilities = (date, numberOfDays) => {
  let availabilities = new Map();
  for (let i = 0; i < numberOfDays; ++i) {
    const tmpDate = moment(date).add(i, DAYS);
    availabilities.set(tmpDate.format(YEAR_MONTH_DAY_FORMAT), {
      date: tmpDate.toDate(),
      slots: []
    });
  }
  return availabilities;
};


/**
 * Event Start date Format with momentJs
 * @param event
 * @returns {moment.Moment}
 */
const getStartDate = (event) => {
  return moment(event.starts_at);
};


/**
 * Checking the given date is before than end date or not
 * @param date
 * @param event
 * @returns {boolean}
 */
const isBeforeEventEndDate = (date, event) => {
  return date.isBefore(event.ends_at);
};


/**
 * Minutes increment with the given timestamps
 * @param date
 * @param minutes
 * @returns {*}
 */
const minuteIncrement = (date, minutes = DEFAULT_MINUTES_INCREMENT) => {
  return date.add(minutes, MINUTES);
};


/**
 * Date format with given format style with momentJS
 * @param date
 * @param format
 * @returns {string}
 */
const formatDate = (date, format) => {
  return moment(new Date(date)).format(format)
};


/**
 * Pushing the new booking slot in a specific day
 * @param availabilities
 * @param availableDateKeys
 * @param date
 */
const daySlotBook = (availabilities, availableDateKeys, date) => {
  availableDateKeys.forEach(key => {
    const day = availabilities.get(key);
    if (day.slots.indexOf(date.format(TIMESTAMP_FORMAT)) < 0) {
      day.slots.push(date.format(TIMESTAMP_FORMAT));
    }
  });
};


/**
 * Filter slots when APPOINTMENT request
 * @param availabilities
 * @param availableDateKeys
 * @param date
 */
const daySlotFilter = (availabilities, availableDateKeys, date) => {
  availableDateKeys.forEach(key => {
    const day = availabilities.get(key);
    day.slots = day.slots.filter(
        slot => slot.indexOf(date.format(TIMESTAMP_FORMAT)) === -1
    );
  });
};


/**
 * Weekly Recurring Filtering and get the dateKeys according with it
 * @param availabilitiesKeys
 * @param date
 * @param isWeeklyRecurring
 * @returns {Array}
 */
const getDateKeysOnSpecificDate = (availabilitiesKeys, date, isWeeklyRecurring = false) => {
  let availableDateKeys = [];
  availabilitiesKeys.forEach(dateKey => {
    if (isWeeklyRecurring && date.format(DAY_FORMAT) === formatDate(dateKey, DAY_FORMAT))
      availableDateKeys.push(dateKey);
    else if (!isWeeklyRecurring && date.format(YEAR_MONTH_DAY_FORMAT) === formatDate(dateKey, YEAR_MONTH_DAY_FORMAT))
      availableDateKeys.push(dateKey);
  });
  return availableDateKeys;
};

/**
 * Check Opening and appointment availabilities with the given date
 * @param event
 * @param availabilities
 * @param date
 */
const checkAvailabilities = (event, availabilities, date) => {
  const availabilitiesKeys = Array.from(availabilities.keys());
  const getAvailableDateKeys = getDateKeysOnSpecificDate(availabilitiesKeys, date, event.weekly_recurring);

  if (event.kind === OPENING) daySlotBook(availabilities, getAvailableDateKeys, date);
  else if (event.kind === APPOINTMENT) daySlotFilter(availabilities, getAvailableDateKeys, date);
};


/**
 * Finding the availabilities
 * @param date
 * @param numberOfDays
 * @returns {Promise<any[]>}
 */
export default async function getAvailabilities(date, numberOfDays = DEFAULT_NUMBER_OF_DAYS) {
  const availabilities = setAvailabilities(date, numberOfDays);
  const events = await getEventsFilterByDate(date);
  for (const event of events) {
    for (let date = getStartDate(event); isBeforeEventEndDate(date, event); minuteIncrement(date)) {
      checkAvailabilities(event, availabilities, date);
    }
  }
  return Array.from(availabilities.values())
}