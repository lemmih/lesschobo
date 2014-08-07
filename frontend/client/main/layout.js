Template.layout.events({
  'click #logout': function () {
    console.log('logout');
    Meteor.logout();
  }
});

Template.layout.isOnPage = function (page) {
  var curPage = Router.current().route.name;
  if( page === curPage ) return 'active';
  return null;
};
