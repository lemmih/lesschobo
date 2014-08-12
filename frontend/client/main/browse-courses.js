Template.browseCourses.courses = function () {
  return Courses.find({}).fetch();
};
