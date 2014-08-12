testCollection = new Meteor.Collection('test');

Courses = new Meteor.Collection('courses');

/*
Courses JSON

{ _id: id
, title: text
, slug: text
, description: text
, units: [] }

Units JSON
{ _id: id
, title: text }


Courses Status JSON
{ userId
, courseId
, status: new/seen/mastered
, percentage }

Units Status JSON
{ userId
, unitId
, }

*/
