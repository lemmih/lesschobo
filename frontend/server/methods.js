var path = 'http://localhost:8000';

Meteor.methods({
  fetchReviewCards: function (courseId) {
    var ret = HTTP.call(
      'GET',
      path + '/users/' + Meteor.userId() + '/courses/' + courseId + '/review');
    console.log('review', courseId, JSON.parse(ret.content));
    return JSON.parse(ret.content);
  },
  fetchStudyCards: function (courseId, unitIdx) {
    console.log('path', path + '/users/' + Meteor.userId() + '/courses/' + courseId + '/' + unitIdx);
    var ret = HTTP.call(
      'GET',
      path + '/users/' + Meteor.userId() + '/courses/' + courseId + '/' + unitIdx);
    console.log('study', courseId, JSON.parse(ret.content));
    return JSON.parse(ret.content);
  },
  // fetchAnnotatedStencils: function (courseId) {
  //   console.log('userId', Meteor.userId());
  //   var ret = HTTP.call('GET', 'http://localhost:8000/users/' + Meteor.userId() + '/units/' + courseId + '/stencils/');
  //   return JSON.parse(ret.content);
  // },
  postResponse: function(response) {
    response.userId = Meteor.userId();

    return HTTP.call('POST', 'http://localhost:8000/responses', {data: response, params: {key: 'value'}});
  },

  putCourse: function(course) {
    // FIXME: Fail if course is malformed.
    // XXX: Limit this call to admins/authors.
    HTTP.call(
      'PUT',
      path+'/courses/'+course._id,
      {data: _.map(course.units, function(u){ return u._id; })});
  },
  putUnit: function(unitId, stencils) {
    // XXX: Limit this call to admins/authors.
    HTTP.call(
      'PUT',
      path+'/units/'+unitId,
      {data: stencils});
  }
});

