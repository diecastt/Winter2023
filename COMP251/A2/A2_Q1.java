import java.util.*;

public class A2_Q1{

	public static int game_recursion(int[][] board) {
		return recursion(board, 1);
	}

	public static int recursion(int[][] board, int player){
		ArrayList<Integer> scores = new ArrayList<Integer>();
        int loop = 1;

		for (int i = 0; i < board.length; i++){
			for (int j = 0; j < board[i].length; j++){

				// if found 0, look for possible moves
				if (board[i][j] == 0){

					// looking for possible move to the right
					if (j + 2 < board[i].length && board[i][j + 2] > 0 && board[i][j + 1] > 0){
						int current_move = player * board[i][j + 2] * board[i][j + 1];

						//update board with the move
						int source = board[i][j + 2];
						int mid = board[i][j + 1];

						board[i][j] = board[i][j + 2];
						board[i][j + 2] = 0;
						board[i][j + 1] = 0;

						scores.add(current_move + recursion(board, -1 * player));

						// backtrack and reset board
						board[i][j] = 0;
						board[i][j + 2] = source;
						board[i][j + 1] = mid;
					}
					// looking for possible move to the left
					if(j - 2 >= 0 && board[i][j - 2] > 0 && board[i][j - 1] > 0){
						int current_move = player * board[i][j - 2] * board[i][j - 1];

						//update the board with the move
						int source = board[i][j - 2];
						int mid = board[i][j - 1];

						board[i][j] = source;
						board[i][j - 2] = 0;
						board[i][j - 1] = 0;

						scores.add(current_move + recursion(board, -1 * player));

						// backtrack and reset board
						board[i][j] = 0;
						board[i][j - 2] = source;
						board[i][j - 1] = mid;
					}
					// looking for possible move to right upper diagonal
					if(i - 2 >= 0 && board[i - 2][j] > 0 && board[i - 1][j] > 0){
						int current_move = player * board[i - 2][j] * board[i - 1][j];

						//update the board with the move
						int source = board[i - 2][j];
						int mid = board[i - 1][j];

						board[i][j] = source;
						board[i - 2][j] = 0;
						board[i - 1][j] = 0;

						scores.add(current_move + recursion(board, -1 * player));

						// backtrack and reset board
						board[i][j] = 0;
						board[i - 2][j] = source;
						board[i - 1][j] = mid;
					}
					// looking for possible move to lower left diagonal
					if(i + 2 < board.length && board[i + 2][j] > 0 && board[i + 1][j] > 0){
						int current_move = player * board[i + 2][j] * board[i + 1][j];

						//update the board with the move
						int source = board[i + 2][j];
						int mid = board[i + 1][j];

						board[i][j] = source;
						board[i + 2][j] = 0;
						board[i + 1][j] = 0;

						scores.add(current_move + recursion(board, -1 * player));

						// backtrack and reset board
						board[i][j] = 0;
						board[i + 2][j] = source;
						board[i + 1][j] = mid;

					}
					// looking for possible move to upper left diagonal
					if (i - 2 >= 0 && j  - 2 >= 0 && board[i - 2][j - 2] > 0 && board[i - 1][j - 1] > 0){
						int current_move = player * board[i - 2][j - 2] * board[i - 1][j - 1];

						//update the board with the move
						int source = board[i - 2][j - 2];
						int mid = board[i - 1][j - 1];

						board[i][j] = source;
						board[i - 2][j - 2] = 0;
						board[i - 1][j - 1] = 0;

						scores.add(current_move + recursion(board, -1 * player));

						// backtrack and reset board
						board[i][j] = 0;
						board[i - 2][j - 2] = source;
						board[i - 1][j - 1] = mid;
					}
					// looking for possible move to lower right diagonal
					if(i + 2 < board.length && j + 2 < board[i].length && board[i + 2][j + 2] > 0 && board[i + 1][j + 1] > 0){
						int current_move = player * board[i + 2][j + 2] * board[i + 1][j + 1];

						//update the board with the move
						int source = board[i + 2][j + 2];
						int mid = board[i + 1][j + 1];

						board[i][j] = source;
						board[i + 2][j + 2] = 0;
						board[i + 1][j + 1] = 0;

						scores.add(current_move + recursion(board, -1 * player));

						// backtrack and reset board
						board[i][j] = 0;
						board[i + 2][j + 2] = source;
						board[i + 1][j + 1] = mid;
					}
				}
				continue;

			}
		}
		if (scores.size() == 0){
			return 0;
		}
		else if (player == -1){
			return Collections.min(scores);
		}
		return Collections.max(scores);
	}

	public static void main(String[] args) {
		int[][] board = {
				{0, -1, -1, -1, -1},
				{1, 1, -1, -1, -1},
				{1, 1, 1, -1, -1},
				{1, 1, 1, 1, -1},
				{1, 1, 1, 1, 1}
		};

		System.out.println(game_recursion(board));
	}
}
