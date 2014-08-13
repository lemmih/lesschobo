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
