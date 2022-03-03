package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }




    protected void initStructures()
    {
        pVisited = new ConcurrentSkipListSet<>();
        pPredecessor = new ConcurrentSkipListMap<>();
        pFrontier = new LinkedBlockingDeque<>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    protected ConcurrentSkipListSet<Integer> pVisited;               //Thread safe data structures

    protected ConcurrentSkipListMap<Integer, Integer> pPredecessor;

    protected BlockingDeque<Integer> pFrontier;



    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
        {
            //SequentialSolver ss = new SequentialSolver(Maze maze);
            // one player active on the maze at start
            int player = maze.newPlayer(start);
            // start with start node
            pFrontier.push(start);
            // as long as not all nodes have been processed
            while (pFrontier.size() != 0) {
                // get the new node to process
                int current = pFrontier.pop();
                // if current node has a goal
                if (maze.hasGoal(current)) {
                    // move player to goal
                    maze.move(player, current);
                    // search finished: reconstruct and return path
                    return pPathFromTo(start, current);
                }
                // if current node has not been visited yet
                if (!pVisited.contains(current)) {
                    // move player to current node
                    maze.move(player, current);
                    // mark node as visited
                    pVisited.add(current);
                    // for every node nb adjacent to current
                    for (int nb : maze.neighbors(current)) {

                        //TODO
                        //If neighbor amount > 1, fork for every neighbor

                        // add nb to the nodes to be processed
                        pFrontier.push(nb);
                        // if nb has not been already visited,
                        // nb can be reached from current (i.e., current is nb's predecessor)
                        if (!pVisited.contains(nb))
                            pPredecessor.put(nb, current);
                    }
                }
            }
            // all nodes explored, no goal found
            return null;
        }
    }
    protected List<Integer> pPathFromTo(int from, int to) {
        CopyOnWriteArrayList<Integer> path = new CopyOnWriteArrayList<>();
        Integer current = to;
        while (current != from) {
            path.add(current);
            current = pPredecessor.get(current);
            if (current == null)
                return null;
        }
        path.add(from);
        Collections.reverse(path);
        return path;
    }
}
