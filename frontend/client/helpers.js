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
};

getTempUserId = function () {
  return Cookie.get('temp-userid');
};
Meteor.startup(function () {
  var thisId = Cookie.get('temp-userid') || Random.id();
  Cookie.set('temp-userid', thisId);
});

