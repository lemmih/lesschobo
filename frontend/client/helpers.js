Handlebars.registerHelper('key_value', function(context, options) {
  var result = [];
  _.each(context, function(value, key, list){
    result.push({key:key, value:value});
  })
  return result;
});

Meteor.Collection.prototype.empty = function () {
  return !this.findOne({});
}
Meteor.Collection.prototype.insertMany = function (lst) {
  for(var i=0; i < lst.length; i++)
    this.insert(lst[i]);
}
Meteor.Collection.prototype.copy = function (c) {
  this.insertMany(c.find({}).fetch());
}

// This helper template is needed because meteor doesn't work
// well with the HTML5 autofocus tag. Instead, we use jquery
// to focus the element when the template is rendered.
Template.autofocus.rendered = function () {
  $(".autofocus").focus();
  if( $('.autofocus').attr('data-content') ) {
    $('.autofocus').popover({trigger: 'focus'});
    $('.autofocus').popover('show');
  }
};
Template.autofocus.destroyed = function () {
  if( $('.autofocus').attr('data-content') ) {
    $('.autofocus').popover('destroy');
  }
}

var templateUniqueId = 0;
Template.expandable.created = function () {
  this.templateInstanceId = templateUniqueId++;
  var ident = 'visible'+this.templateInstanceId;
  Session.set(ident, false);
}
Template.expandable.toggle = function () {
  var self = UI._templateInstance().templateInstanceId;
  return Session.equals('visible'+self, false);
}
Template.expandable.unique = function () {
  return UI._templateInstance().templateInstanceId;
}
Template.expandable.events({
  'click a': function (evt, tmpl) {
    var ident = 'visible'+tmpl.templateInstanceId;
    Session.set(ident, Session.equals(ident, false));
  }
});

getTempUserId = function () {
  return Cookie.get('temp-userid');
};
Meteor.startup(function () {
  var thisId = Cookie.get('temp-userid') || Random.id();
  Cookie.set('temp-userid', thisId);
});




Template.dotdotdot.rendered = function () {
  this.$(".dotdotdot").dotdotdot({watch: true});
};
Template.dotdotdot.destroyed = function () {
}
Template.dotdotdot.events({
  'click .dotdotdot': function (evt) {
    if( $(evt.target).hasClass('clicked') ) {
      $(evt.target).removeClass('clicked');
      $(evt.target).dotdotdot({watch: true});
    } else {
      $(evt.target).addClass('clicked');
      $(evt.target).trigger('destroy.dot');
    }
  }
});