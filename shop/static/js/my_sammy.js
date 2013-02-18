(function($) {
    var app = $.sammy(function() {

        this.use(Sammy.Handlebars);
        this.use(Sammy.Template);
        this.use(Sammy.JSON);
        this.use(Sammy.Storage);
        this.use(Sammy.Session);

        var store = new Sammy.Store({name: 'mystore', element: '1', type: 'local'});
        var login = new Sammy.Store({name: 'login', element: '2', type: 'local'});
        var cache = new Sammy.Store({name: 'cache', element: '3', type: 'memory'});
        this.use('Handlebars', 'mustache');
        this.disable_push_state = true;
        this.clearTemplateCache();
        //this.setLocationProxy(new Sammy.PushLocationProxy(this));
        //this.setLocationProxy(new Sammy.DataLocationProxy(this, 'location', 'rel'));



        this.bind('flash', function(e, message) {
            $('#flashMsg').html(message);
            $('#flash').fadeIn('slow').delay(5000).fadeOut('slow');
        });


// based on the `#each` helper, requires jQuery (for jQuery.extend)
Handlebars.registerHelper('each_hash', function(context, options) {
    var fn = options.fn, inverse = options.inverse;
    var ret = "";

    if(typeof context === "object") {
        for(var key in context) {
            if(context.hasOwnProperty(key)) {
                // clone the context so it's not
                // modified by the template-engine when
                // setting "_key"
                var ctx = jQuery.extend(
                    {"_key":key},
                    context[key]);

                ret = ret + fn(ctx);
            }
        }
    } else {
        ret = inverse(this);
    }
    return ret;
});


Handlebars.registerHelper('eachname', function(context, options) {
  var ret = [];

  if (context) {
      ret ="another"
      for(i in context) {
          alert(i);
          ret = ret.push (i);
      }
  }

  return ret;
});


Handlebars.registerHelper('attachNames', function(items) {
    var res = [];
    if (items) {
        for (i in items) {
            alert(i);
            res = res.push(i);
        }
        return res;
    } else {
        return [];
    }
});



        var current_user = false;

        this.trigger('update-categories');


        function checkLoggedIn() {
            // /session returns a JSON representation of the logged in user
            // or an empty object

	    $.post('/json/checklogin', function(data) {
		if (data == "fail") {
		    login.clear("user");
                }
            if (login.get('user')) {
                $('#menu_login').hide();
                $('#menu_user').show();
                $('.admin-only').show();
                this.admin = login.get("user");
            } else {
                $('#menu_login').show();
                $('#menu_user').hide();
                $('.admin-only').hide();
            }

	    });

        };

	this.get("#/delcategory/:cat", function() {
	    var context = this;
	    $.post("/json/delcategory", this.params, function(response) {
                context.trigger('update-categories');
                //             context.next(JSON.parse(response));
            });
	    this.redirect("#/");
        });

	this.get("#/category/:category", function() {
	    var context = this;
	    var category = this.params['category'];
            var link = "/json/category";
            if (this.params['sort']) {
                link = "/json/categorycost"
            }

            $('#main').empty();
            $('#premain').empty();
            this.load("/json/categories", {"json":true})
		.then(function(categories) {
                    var cat = {};
                    for (i in categories) {
                        if (categories[i].catName == category) {
                            cat = categories[i];
                        }
                    }
                    this.load('/json/tagscategory?category=' + encodeURIComponent(category), {"json":true})
                        .then(function(tags) {
	                    this.load(link + '?category=' + encodeURIComponent(category), {"json":true})
		                .then(function(items) {
		                    $("#main").fadeIn('fast', function() {
                                        context.render('templates/category.mustache',
                                                       {"items":items,
                                                        "tags":tags,
                                                        "catTitle":cat.catTitle,
                                                        "tags":tags,
                                                        "catId":cat.catId,
                                                        "catContent":cat.catContent,
                                                        "catName":category})
			                    .replace('#main')
			                    .then(function () {
				                $("#main").fadeIn('fast');
	                                        $('.nav li').removeClass('active');
				                $('#cat_' + category).addClass('active');
                                                checkLoggedIn();
	                                    });
		                    });
	                        });
                        });

	        });
        });

	this.get("#/categorytag/:category/:tag", function() {
	    var context = this;
	    var category = this.params['category'];
	    var tag = this.params['tag'];
            var link = "/json/categorytag";
            if (this.params['sort']) {
                link = "/json/categorytag"
            }
            var categories = cache.get("categories");
            var cat = {};
            for (i in categories) {
                if (categories[i].catName == category) {
                    cat = categories[i];
                }
            }
            $('#main').empty();
            this.load('/json/tagscategory?category=' + category, {"json":true})
                .then(function(tags) {
                    context.render('templates/category_content.mustache',
                                   {"catTitle":cat.catTitle,
                                    "tags":tags,
                                    "catId":cat.catId,
                                    "catContent":cat.catContent,
                                    "catName":category})
                        .replace('#premain');
                });

	    this.load(link + '?category=' + category + "&tag=" + tag, {"json":true})
		.then(function(items) {
		    $("#main").fadeIn('fast', function() {
                        context.renderEach('templates/item.mustache',items)
			    .replace('#main')
			    .then(function () {
				$("#main").fadeIn('fast');
	                        $('.nav li').removeClass('active');
				$('#cat_' + category).addClass('active');
                                checkLoggedIn();
	                    });
		    });
	    });
	});

	this.around(function(callback) {
	    var context = this;
            app.clearTemplateCache();
//	    $(".alert").alert('close');
	    $('.telephone').text("27-87-28");
	    $('.nav li').removeClass('active');
	    this.categories = cache.get("categories");
            checkLoggedIn();
	    callback();
	 });


        this.get("#/editpage/:id", function() {
	    var id = this.params['id'];
            var context = this;
            $('#premain').empty();

            this.contentId = id;
            this.load("/json/rawcontent?id=" + id, {"json":true})
		.then(function(items) {
		    $("#main").fadeOut('fast', function() {
			context.render('templates/edit_content.mustache',items)
			       .replace('#main');
			$("#main").fadeIn(500);
		    });
		});
        });
	this.post("#/editpage/:id", function() {
	    $.post("/json/editcontent", this.params, function(response) {
                //             context.next(JSON.parse(response));
            });
	    this.redirect("#/");
	});

	this.get("#/deleteitem/:category/:id", function() {
	    var context = this;
            var cat = this.params['category'];
            var id = this.params['id'];
	    $.post("/json/delitem", {"id": id}, function(response) {
                if (response == "ok") {
                    context.redirect("#/category/" + cat);
                }
                //history.back();
                //context.next(JSON.parse(response));
            });
	});

	this.get("#/image/delete/:id/:image", function() {
	    var context = this;
            var image = this.params['image'];
            var id = this.params['id'];
	    $.post("/json/delimage", {"id": id, 'image': image}, function(response) {
                if (response == "ok") {
                    $("#image_"+image).hide();
                }
                //history.back();
                //context.next(JSON.parse(response));
            });
            this.redirect("#/edit/" + id);
	});

	this.get("#/deletereq/:id", function() {
	    var context = this;
            var id = this.params['id'];
	    $.post("/json/delitem", {"id": id}, function(response) {
                if (response == "ok") {
                    context.redirect("#/requests");
                }
                //history.back();
                //context.next(JSON.parse(response));
            });
	});

        this.get("#/edit/:id", function() {
	    var id = this.params['id'];
            var context = this;
            $('#premain').empty();
	    this.load('/json/categories', {"json":true})
		.then(function(categories) {
                    this.load("/json/item/?id=" + id, {"json":true})
		        .then(function(item) {
                            for (i in categories) {
                                if (categories[i].catName == item.category) {
                                    categories[i].isSelected = true;
                                }
                            }
                            item.categories = categories;
		            $("#main").fadeOut('fast', function() {
			        context.render('templates/edit_item.mustache',item)
			            .replace('#main');
			        $("#main").fadeIn(500);
		            });
		        });
		});
        });

        this.post("/json/edit", function(){
            var context = this;
	    $.post("/json/edit", this.params, function(response) {
                context.trigger('flash', "Запись обновлена...");
                app.runRoute("get", "#/edit/" + context.params['id']);
                //             context.next(JSON.parse(response));
            });
        });
	this.post("#/edit", function() {
            var context = this;
	    $.post("/json/edit", this.params, function(response) {
                context.trigger('flash', "Запись обновлена...");
                app.runRoute("get", "#/edit/" + context.params['id']);
                //             context.next(JSON.parse(response));
            });
//	    this.redirect("#/");
	});

        this.get("#/edit_category/:id", function() {
	    var id = this.params['id'];
            var context = this;
            $('#premain').empty();
            this.load("/json/getcategory?id=" + id, {"json":true})
		.then(function(items) {
		    $("#main").fadeIn('fast', function() {
			context.render('templates/edit_cat.mustache',items)
			       .replace('#main');
			$("#main").fadeIn(500);
		    });
		});
	    this.render('templates/edit_cat.mustache',cat)
		.replace('#main');
        });

	this.post("#/edit_category/", function() {
            var context = this;
	    $.post("/json/editcategory", this.params, function(response) {
                if (response == "ok") {
                    context.trigger('update-categories');
                    context.redirect("#/category/" + context.params['catName']);
                }

//             context.next(JSON.parse(response));
           });
	});

        this.get("#/search", function () {
            var str = this.params['str'];
            var context = this;
            $('#premain').empty();
            // FIXME: maybe race condition
            this.load("/json/searchitem?str=" + str, {"json":true})
		.then(function(items) {
                    var cat = {};
	            var categories = cache.get("categories");
                    for (j in items) {
                        for (i in categories) {
                            if (categories[i].catName == items[j].category) {
                                items[j].catTitle = categories[i].catTitle;
                            }
                        }
                    }
		    $("#main").fadeOut('fast', function() {
			context.renderEach('templates/search_item.mustache',items)
			       .replace('#main');
			$("#main").fadeIn(500);
		    });
		});
        });

	this.get("#/logout", function() {
	    var context = this;
	    $.post('/json/logout', {}, function(data) {
		login.clear("user");
		context.redirect("#/");
	    });
	});

	this.post("#/login", function() {
	    var pass = this.params['pass'];
	    var context = this;

	    $.post('/json/login', {pass:pass}, function(data) {
		if (data == true) {
		    $('#myModal').modal('hide');
		    login.set("user", "Админ");
		    context.redirect("#/");
		}
	    });
	});

//////////////////////////////////
// STATIC
/////////////////////////////////
	this.get("#/discount", function() {
            $('#premain').empty();
	    $('#menu_discount').addClass('active');
            this.render('templates/main.mustache', {"contentId":"discount"})
                .replace("#main");
	    this.load("/json/content?id=discount", {"json":true})
		.render('templates/main.mustache')
		.replace('#main')
		.then(function () {
                    checkLoggedIn();
	        });
	});

	this.get("#/deliver", function() {
            $('#premain').empty();
	    $('#menu_deliver').addClass('active');
            this.render('templates/main.mustache', {"contentId":"deliver"})
                .replace("#main");
	    this.load("/json/content?id=deliver", {"json":true})
		.render('templates/main.mustache')
		.replace('#main')
		.then(function () {
                    checkLoggedIn();
	        });
	});

	this.get("#/contacts", function() {
            $('#premain').empty();
	    $('#menu_contacts').addClass('active');
            this.render('templates/main.mustache', {"contentId":"contacts"})
                .replace("#main");
	    this.load("/json/content?id=contacts", {"json":true})
		.render('templates/main.mustache')
		.replace('#main')
		.then(function () {
                    checkLoggedIn();
	        });
	});

	this.get("#/address", function() {
            $('#premain').empty();
	    $('#menu_address').addClass('active');
            this.render('templates/main.mustache', {"contentId":"address"})
                .replace("#main");
	    this.load("/json/content?id=address", {"json":true})
		.render('templates/main.mustache')
		.replace('#main')
		.then(function () {
                    checkLoggedIn();
	        });
        });

	this.get("#/page/:page", function() {
            var page = this.params['page'];
            $('#premain').empty();
	    $('#menu_' + page).addClass('active');
	    this.load("/json/content?id=" + page, {"json":true})
                .then(function(items) {
                    if (items == null)
                    {
                        this.render('templates/main.mustache', {"contentId":page})
                            .replace("#main")
		            .then(function () {
                                checkLoggedIn();
	                    });

                    } else {
		        this.render('templates/main.mustache',items)
		            .replace('#main')
		            .then(function () {
                                checkLoggedIn();
	                    });
                    }
                });

        });

        this.get("#/additem", function() {
            var context = this;

            $('#premain').empty();
            this.trigger('update-catregories');
	    this.render('templates/additem.template')
		.replace('#main');
            // FIXME: update trigger before render
        });

        this.post("/json/additem", function(){
            var context = this;
            var category = this.params.category;

            $("#addform").ajaxSubmit({
                url: '/json/additem',
                success: function() {
                    context.trigger('flash', "Запись добавлена.");
                    context.redirect("#/category/" + category);
                    app.runRoute("get", "#/category/" + category);
                }
            });
        });

        this.post("#/additem", function(){
            var context = this;
            var category = this.params.category;

            $("#addform").ajaxSubmit({
                url: '/json/additem',
                success: function() {
                    context.trigger('flash', "Запись добавлена.");
                    context.redirect("#/category/" + category);
                    app.runRoute("get", "#/category/" + category);
                }
            });
        });

        this.get("#/feedbacks", function() {
            var context = this;
            $('#premain').empty();

	    this.load("/json/feedbacks", {"json":true})
		.renderEach('templates/feedbacks.mustache')
		.replace('#main');
        });


        this.get("#/addcat", function() {
            var context = this;
            $('#premain').empty();
	    this.render('templates/addcat.template')
		.replace('#main');
        });

        this.post("#/addcat", function() {
            var context = this;
            var categories = cache.get("categories");

            for (i in categories) {
                if (categories[i].catName == this.params.catName) {
                    alert("Такой раздел уже есть...");
                    return;
                }
            }

            $.post('/json/addcat', this.params, function(data) {
                context.trigger('update-categories');
                context.redirect("#/");
            });
        });

        this.get("#/requests", function() {
            var context = this;
            var totalCost = 0;
            this.render('templates/requests_content.mustache')
                .replace('#premain');
            this.load("/json/requests", {"json":true})
                .then(function (items) {
                    for (i in items) {
                        var totalCost = 0;
                        key1 = items[i].requestItems;
                        for (j in key1) {
                            key2 = key1[j];
                            key2.total = parseInt(key2.requestCost,10)
                                * parseInt(key2.requestCount,10);
                            if (key2.total)
                                totalCost += key2.total;
                        }
                        items[i].totalCost = totalCost;
                    }
                    $.post("/json/readrequests");
                    context.renderEach('templates/item_requests.mustache', items)
                        .replace('#main');
                });
        });

        this.get("#/requests2", function() {
            $('#premain').empty();
            this.redirect("#/requests");
        });
        this.get("#/newrequests", function() {
            $('#premain').empty();
            this.render('templates/requests_content.mustache')
                .replace('#main');
        });
        this.get("#/oldrequests", function() {
            $('#premain').empty();
            this.redirect("#/requests");
        });

        this.post("#/checkout", function() {
            var context = this;
	    var items   = [];
	    var counts  = [];
	    $.each(store.keys(), function(i) {
		var key = store.keys()[i];
		var value = store.get(key);
		items.push(key);
		counts.push(value.count);
	    });
	    this.params.items  = items.join(',');
	    this.params.counts = counts.join(',');
	    $.post('/json/checkout', this.params, function(data) {
		if (data == "ok") {
		    store.clearAll();
		    // store the cart
		    context.trigger('update-cart');
		    context.redirect("#/");
		} else {
		    alert(data);
		}
	    });
        });

	this.get('#/cart', function(context) {
	    var context = this;
            $('#premain').empty();

	    this.render('templates/cat_content.template')
		.replace('#main');
	    $.each(store.keys(), function(i) {
		var key = store.keys()[i];
		var value = store.get(key);
		context.load("/json/item/?id=" + key, {"json":true})
		       .then(function (items) {
			   items.count = value.count;
			   context.render('templates/cart_item.template', items)
			          .appendTo('#main');
		       });
	    });
	});

	this.post('#/cart', function(context) {
	    var item_id = this.params['id'];
	    var message = "Товар добавлен в корзину...";
	    $('#item_add').html('<div class="alert alert-success"><a class="close" data-dismiss="alert">×</a><span>'+message+'</span></div>');
	    // fetch the current cart
	    var old_count = 0;
	    if (store.exists(item_id)) {
		    old_count = store.get(item_id).count;
	    }
	    store.set(item_id, { count: old_count + 1
			       , cost: parseInt(this.params['cost'], 10)});
	    // store the cart
	    this.trigger('update-cart');
	});

	this.post('#/cart_delete', function(context) {
	    var item_id = this.params['id'];
	    // fetch the current cart

	    store.clear(item_id);
	    // store the cart
	    this.trigger('update-cart');
	    this.redirect('#/cart');
	});

	this.get('#/cart_clear', function(context) {
	    // fetch the current cart

	    store.clearAll();
	    // store the cart
	    this.trigger('update-cart');
	    this.redirect('#/cart');
	});


        this.get("", function() {
	    $('#menu_main').addClass('active');

            $('#premain').empty();
	    this.trigger('update-categories');

            var context = this;
            this.id = "main";
            this.contentId = "main";
            this.render('templates/main.mustache', {"contentId":"main"})
                .replace("#main");
	    this.load("/json/content?id=main", {"json":true})
		.render('templates/main.mustache')
		.replace('#main')
		.then(function () {
                    checkLoggedIn();
	        });
        });


//////////////////////////////////////////


	this.bind('update-cart', function() {
	    var sum = 0;
	    keys = store.keys();
	    $.each(keys, function(i) {
		var value = store.get(keys[i])
		sum += value['cost']*value['count'];
	    });
	    $('.cart-info')
		.find('.cart-items').text(sum).end();
	});

	this.bind('update-categories', function() {
	    var context = this;
	    // Display categories
	    this.load('/json/categories', {"json":true})
		.then(function(items) {
		    cache.set("categories", items);
		    this.renderEach('templates/categories.template',items)
			.replace('#categories');
		});
	});

        this.bind('update-requests', function() {
            var context = this;
            if (login.get('user')) {
            // Display categories
                this.load('/json/newrequests', {"json":true})
                    .then(function(items) {
                        var count = items.length;
                        if (context.oldCount != count && count != 0) {
                            $("#newRequestsAlert").show();
                        }
                        context.oldCount = count;
                        $("#requestNumber").text(count);
                        setTimeout(function() {context.trigger('update-requests');}, 50000);
                    });
            }
        });



	this.bind('clear-cart', function() {
	    store.clear();
	});

        this.bind('run', function() {
            // initialize the cart display
            this.trigger('update-cart');
            this.trigger('update-categories');
            this.trigger('update-requests');
        });



	// this.around(function(callback) {
        //     var context = this;
        //     this.load('/testapp/')
	// 	.then(function(items) {
	// 	    context.items = items;
	// 	})
	// 	.then(callback);
	// });


	// use rows
    });


    $(function() {
        app.run('#/')
    });
})(jQuery);
