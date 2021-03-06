===================
CNC Pattern Library
===================

A Haskell library/framework for creating tiling patterns for CNC routers. `cnc-pattern-lib` was inspired by `Sam Calisch's <http://samcalisch.com/>`_ `fold <https://github.com/calischs/fold/>`_ library for Python. This library creates SVG files, which can be read directly by many CNC routing software packages, and there are `open source programs <https://github.com/avwuff/SVG-to-GCode>`_ that allow you to convert between SVG and other common formats like `G-Code <https://en.wikipedia.org/wiki/G-code>`_

* Visit the `gallery <https://mazelife.github.io/cnc-pattern-lib/index.html>`_ to see all included patterns.
* View the haddock-based `documentation <https://mazelife.github.io/cnc-pattern-lib/haddock/>`_ for this library.

.. image:: docs/router.jpg
	:align: center


Getting Started
---------------

This library is designed to work with `Stack <https://docs.haskellstack.org/en/stable/README/>`_, a cross-platform program for developing Haskell projects, so you should start by making sure you have that installed.

You can check that everything works as expected by running ``stack test``, which should (sucessfully) run the project's tests. ``cnc-pattern-lib``. To start generating SVG files, build the ``cnc-pattern-lib`` executable::

	$> stack install

This will compile the code, build an executable, and copy the generated executable to your local bin path (e.g. ``~/.local/bin`` on OS X). The ``cnc-pattenr-lib`` executable is self-documenting through its ``--help`` flag::


	$> cnc-pattern-lib - a program to render SVG files for CNC routers

	Usage: cnc-pattern-lib SCENE [--preview]
	Render a scene as an SVG file.

	Available options:
	SCENE                    Name of scene to render. One of: hexagons,
							hoberman-cylinder, huffman-tower, resch, simple,
							triangles
	--preview                Show SVG in preview window.
	-h,--help                Show this help text

We can render one of the available scenes (i.e. SVG files) thusly::

	$> cnc-pattern-lib huffman-tower > huffman-tower.svg

You can then take a look at the output file ``huffman-tower.svg`` in you SVG viewer of choice.

Shape Primitives
----------------

This library provides the following shape primitives:

Point
^^^^^

Strictly speaking, not a shape. But the ``Point`` primitive allows you to create shapes::

	aPoint = Point 1 3.5

This creates a point at the coordinates (1, 3.5) in your scene. Note that (0,0) is the exact center of the scene and the overall width and height are set by you.

Arc
^^^

You define an arc thusly::

	center = Point 0 0
	someArc = Arc center 3.5 2 3

This will create an arc with a center point at 0,0 in your scene with a radius of 3.5, going from 2 to 3 radians.

Circle
^^^^^^

You define a circle thusly::
		
	someCircle = Circle (Point 0 0) 3.5

This will create a circle with a center point at 0,0 in your scene with a radius of 3.5.

Hexagon
^^^^^^^

You define a hexagon thusly::
		
	someHex = Hexagon (Point 0 0) 3.5

This will create a circle with a center point at 0,0 in your scene with a "radius" (line from center to the edge at a 0-degree angle) of 3.5.


Line
^^^^

You define a line thusly::
		
	someLine = Line (Point 0 0) (Point 1 2.3)

This will a line from 0,0 to 1,2.3 in your scene.


Rectangle
^^^^^^^^^

You define a rectangle (really a parallelogram) thusly::
		
	someRect = Rectangle (Point 1 2) (Point 3 2) (Point 3 0) (Point 1 0)

This will create a rectangle with four corners (going top left, top right, bottom right, bottom left) at (1,2) (3, 2) (3, 0), (1, 0). Strictly speaking, you only need to provide a top-left and bottom right corner to define a rectangle so there's a shortcut for that::

	someRect = mkRectangle (Point 1 2) (Point 3 0).

		
Shape Typeclasses
-----------------

SvgShape
^^^^^^^^

All shapes implement ``SvgShape``, meaning you can call ``toSvg anyShape`` to get a `blaze SVG <https://hackage.haskell.org/package/blaze-svg>`_ representation.


Transformable
^^^^^^^^^^^^^

All shapes support a set of geometric transformations:

* ``translate  anyShape (Point 1 2)``: Move a shape in space adding the given point to the X and Y coordinates.
* ``rotate     anyShape (Point 1 2) 3.4``: Rotate a shape about a line through point p along vector t.
* ``mirror     anyShape (Point 1 2) (Point 3 2)``:  Mirror a shape about a line through point p along vector v
* ``offset     anyShape (Point 1 2) True``: Offset a shape left (``True``) or right (``False``).

Mergable
^^^^^^^^

If two shapes can be merged into one, return the merged shape::

	lineA = Line (Point 0 0) (Point 2 2)
	lineB = Line (Point 1 1) (Point 3 3)
	merge lineA lineB

This will return ``Just Line (Point 0 0) (Point 3 3)`` in this case, or may return ``Nothing`` if they aren't mergable.

A list of shapes can also be merged like so: ``optimize [lineA, lineB]``, which will return a new list where all shapes that can be merged have been, plus all shapes that couldn't.

Groups, Layers, and Scenes
--------------------------

You can organize sets of shapes using a ``Group`` or a ``Layer``. Shapes, groups, and layers can all be stored in a ``Scene``, which is equivalent to a Single SVG file. Scenes have a height, a width (both measured in inches) and a style.

Let's see an example of these concepts at work::

	import Circle
	import Point

	circle = Circle (Point 0 0) 1

We've created a circle at the center of our canvas with a radius of 1 inches. Now let's create a set of circles based on transformations of the original::

	circleList  = [circle, translateP circle (Point 1 0), translateP circle (Point (-1) 0)]

Our ``circleList`` has the original circle, a circle moved to the right by 1, and a circle to the left by 1. Now let's put those circles in a group::


	import Group

	circles = Group circleList


The ``Group`` type is the functional equivalent of the ``<g>`` `container type <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/g>`_ in the SVG spec, and will be rendered as such.

We can apply transformations to groups too::

	moreCircles = rotate (Point 0 0) (-1.55) circles

This will create a new group that is a rotation of our original group of circles about a line through point (0,0) along vector -1.55. We can also combine groups (since ``Group`` implements ``Semigroup`` and ``Monoid``)::

	allCircles = circles <> moreCircles

This gives us a new group of 6 circles. Another important operation you can perform on a group is to optimize it::

	allCircles = optimizeGroup (circles <> moreCircles) 0.001

This differs from the previous group in that it will remove any duplicate circles from our group. We supply a "tolerance" here (``0.001``) to indicate that if two points differ by less than that distance, they can be treated as functionally equal. Note that the process of optimization works a little differently for different shape primitives:

#. For *circles* and *squares*, duplicates are removed.
#. For *lines* duplicates are removed, but non-duplicate lines can also be combined: Two line segments will be merged if their points are collinear and if one line segment contains at least one of the endpoints of the other.
#. For *arcs* duplicates are removed, but non-duplicate lines can also be combined: Two arcs will be merged if they have the same center point, radius, and if one arc contains at least one of the endpoints of the other.

Applying repeated transformations to groups can sometimes result in duplicate shapes that cause wasted effort by CNC milling equipment (why re-inscribe the same shape multiple times?) so optimization is always a good idea. This example is a case in point. We have a line of three circles with a center point at 0,0. We create a rotated group around point 0,0, meaning that this new group will duplicate one of the circles in the original group.

We can also convert a group to an SVG object::

	toSvg allCircles

In other words, groups are part of the same three typeclasses that shape primitives are: ``SvgShape``, ``Transformable`` and ``Mergable`` and we can do the same things with them. 

Finally let's create a different shape, a square that contains our original circle::

	import Rectangle
	
	square = mkRectangle (Point 1.5 1.5) (Point (-1.5) (-1.5))

At some point we may wish to store our circles and our square in some kind of single container. Groups won't work for this because a group can only contain one kind of shape and we have two. This is where layers come in::

	import Layer

	layer = square +: (toLayer "circles" allCircles) 

Note that layers must be given a name (which is rendered as a comment in the SVG file and can be helpful for debugging). We're using the ``+:`` Layer combinator, which is the same as Haskell's ``cons`` operator (``:``) for lists. Our ``layer`` contains our square and six circles. Layers also belong to the ``SvgShape`` typeclass, meaning we can do ``toSvg layer``. But they don't belong to the ``Transformable`` or ``Mergable`` typeclass [1]. Layers do provide one useful feature which is that we can apply a uniform style to them::

	import Style
	
	someStyle = StyleAttrs {strokeColor=Just "red", strokeWidth=Just 2, fillColor=Just "green"}
	styledLayer = mkLayerWithStyle "red-circles" allCircles someStyle

When rendered, ``styledLayer`` will include stroke color, width, and fill color on all it's objects.

Lastly, let's create a scene with our shapes. A scene can be thought of as the canvas for our pattern. It has a length and width, and can be rendered out to an SVG file. A scene will also have an overall ``Style`` associated with it (although this will be overriden by any layer-specific styles for that layer). Let's create a 5in x 5in canvas containing our layers. Let's say we decide we want all of the shapes in our scene to have a `stroke width <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width>`_ of ``0.5``, and we want the square in our scene to be red and the circles blue. A strightforward way to handle this would be to set the global scene style to have a stroke width of ``0.5`` and a stroke color of blue. And then put our square in a separate layer with it's own style::

	import Scene

	circlesLayer = toLayer "circles" allCircles
	squareLayer = mkLayerWithStyle "square" [square] withStrokeColor "#8c1212"
	globalStyle = StyleAttrs { strokeColor=Just "#121c5b"
                             , strokeWidth=Just 0.05
                             , fillColor=Nothing }

	scene = mkSceneWithStyle 5 5 globalStyle [circlesLayer, squareLayer]

You can see the full version of this scene in the ``Scenes`` submodule as ``Scenes.Simple``. It looks like this when rendered:

.. image:: docs/simple.png


Planned Improvements
--------------------

#. Allow users to specify other units (aside from inches)
#. Implement some further SVG style attributes
#. Add more example patterns

Footnotes
---------

[1] Why can't we merge a layer? Or transform it? Layers represent a heterogeneous collection of types, which are implemented here using Haskell's `existential types <https://wiki.haskell.org/Existential_type>`_. Existential types pack up a value with operations on that value, and hide the actual value's types. What this means is we can't specialize a type once we've packed it up in a type (here called ``ShapeLike``).
