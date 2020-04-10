**Q1.** Suppose I have a component with 5 children, and each children have 2 of
their own, and each has 3 children. How do I message 4.0.1, given that I
received a message from it, for example?  That is, suppose I received a message
and now I want to query it. How do I retrace its steps?

**A.** Each child knows its ID. When a message is raised, the child could insert its
ID in a list. Every time the message is raised, each parent conses its ID. The
parent component can now retrace its steps to query the child, popping the list
as it goes down in a query chain.

We'll call this message followed by a query by **relay.**

**Q2.** How do I drag a component into or out of a component?

**A.** First, consider that whenever a component begins being dragged (MouseDown
event), it should detach itself from its parent and return to the Canvas. With
the current generator method, it should have its ID regenerated.

When the mouse is released (MouseUp event), we need to insert the Draggable as a
child. For this, we need to record which element the mouse is over (MouseEnter
event).

Additionally, each Draggable will need to be sorted by Z-index on its CSS
according to the component's depth. One thing to discuss is whether the user
should be able to alter this property on the Properties panel (which I believe
they should). For this, it will be necessary a **Query** that updates each child,
either setting the base with the specified index and its children with the new
depth, or setting everything to the same depth,

**Q3.** Are there any problems with using the current ID generator method?

**A.** When we save the components, we should flatten the IDs of each child,
removing the "holes" caused by components being detached. The current generator
ID should be saved as well.

In theory, the user could create an overflow, but that would require an
extraordinary amount of components to be created, which renders it practically
impossible, unless a hack/bot/macro is created.

As an alternative solution, we could generate UUIDs or something similar, and
then we wouldn't need to worry about a generator, or changing IDs when the
parent changes. However, I think this might be overkill for something so
trivial, besides that would give the Draggable an Aff dependency, which is to be
avoided. There would be no real benefit.

**Q4.** What would be a good scheme to generating the AST?

**A.** The Canvas and each Draggable should know:
1. Which HTML tag it contains;
2. What CSS properties it contains;
3. What tag elements it contains;
4. Its children;
5. Its position, which may already be contained in **2.**.

Then a **generateAST** function should be created for the Canvas and each Draggable.
It should be a **Query** for **AST -> a** which recurses all the way into the leaf
children, which will just return their ASTs. Their parent will then generate
its AST using the ones from its children and continue this process until
arriving on the Canvas, which will then create the root of the AST and return.
