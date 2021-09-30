import { connect } from 'react-redux';
import { compose, lifecycle, withProps } from 'recompose';
import moment from 'moment';
import { Button } from 'react-bootstrap';
import Bets from './Bets';
import { fetchGame } from '../actions/games';
import { fetchGameContract, fetchGameBets } from '../actions/bets';
import { getCurrentUser, getGame, getGameBets } from '../reducers';

import '../styles/Game.scss';

const Game = ({ game, homeBets, awayBets }) => {
  return game ? (
    <div className='Game'>
      <section className='team'>
        <div className='img-border'>
          <img
            src={game.teams.home.logo}
            alt='team-logo'
            className='img left'
          />
        </div>
        <h2>{game.teams.home.name}</h2>
        <Bets bets={homeBets} />
      </section>
      <section className='game-details'>
        <span className={`status ${game.fixture.status.short.toLowerCase()}`}>
          {game.fixture.status.long}
        </span>
        <span className='date'>
          {moment(game.fixture.date).format('DD MMM HH:mm YYYY')}
        </span>
        <span className='referee'>Referee: {game.fixture.referee}</span>
        <Button variant='secondary'>Make a bet</Button>
      </section>
      <section className='team'>
        <div className='img-border'>
          <img
            src={game.teams.away.logo}
            alt='team-logo'
            className='img right'
          />
        </div>
        <h2>{game.teams.away.name}</h2>
        <Bets bets={awayBets} />
      </section>
    </div>
  ) : (
    <div></div>
  );
};

const enhancer = compose(
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
      game: getGame(state),
      bets: getGameBets(state),
    }),
    (dispatch) => ({
      fetchGame: (id) => dispatch(fetchGame(id)),
      fetchGameContract: (id, gameId) =>
        dispatch(fetchGameContract(id, gameId)),
      fetchGameBets: (id) => dispatch(fetchGameBets(id)),
    })
  ),
  withProps(({ bets, game }) => ({
    homeBets: bets.filter((bet) => bet.outcome === game.teams.home.id),
    awayBets: bets.filter((bet) => bet.outcome === game.teams.away.id),
  })),
  lifecycle({
    componentDidMount() {
      const gameId = this.props.match.params.id;
      const { fetchGame, fetchGameContract, currentUser } = this.props;
      fetchGame(gameId);
      fetchGameContract(currentUser.value, gameId);
    },
    componentDidUpdate() {
      const { game, fetchGameBets, bets } = this.props;
      if (game.contractId && bets.length === 0) {
        fetchGameBets(game.contractId);
      }
    },
  })
);

export default enhancer(Game);
