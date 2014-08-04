Router.configure({
  layoutTemplate: 'layout'
});
Router.map(function () {
  this.route('landing', {
    path: '/',
    layoutTemplate: null,
    loadingTemplate: 'loading'
  });
  this.route('home', {
    path: '/home/',
    waitOn: function() {
      return Meteor.call('listUnits', function (err, data) {
        console.log('Got unit list', data);
        Session.set('units', data);
        // return {units: data};
      });
    },
    data: function () {
      return {units: Session.get('units')};
    }
  });
  this.route('study', {
    layoutTemplate: null,
    path: '/home/study/:id/:slug/',
    waitOn: function() {
      // Meteor.call('fetchCards', this.params.id, function (err, data) {
      //   console.log('Got data', data);
      //   Session.set('stencilId', data[0].stencilId);
      //   Session.set('sentences', data.shift().sentences);
      //   Session.set('cards', data);
      //   Template.interface.setActive();
      // });
    },
    data: function () {
      return {
        courseId: this.params.id,
      };
    }
  });

  this.route('viewCourse', {
    path: '/courses/:id/:slug/',
    data: function () {
      return {
        courseId: this.params.id,
      };
    }
  });

  this.route('browseCourses', {
    path: '/courses/'
  });
});
