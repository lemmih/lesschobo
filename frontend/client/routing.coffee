ChoboRouter = Backbone.Router.extend
    routes:
      "":                       "go_home",
      "courses/:slug/":         "go_study",
      # "browse/":                "go_browse",


    go_home: ->
      console.log('Go home')
      Session.set('course',null)
      return

    go_study: (slug) ->
      console.log('Go study')
      Session.set('course',slug)
      return

    defaultRoute: (actions) ->
      console.log('default')
      this.navigate('/')
      return

window.Router = new ChoboRouter;

Meteor.startup ->
  Backbone.history.start({pushState: true})
