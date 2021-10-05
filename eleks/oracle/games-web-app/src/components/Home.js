import { connect } from 'react-redux';
import { compose, lifecycle, withProps, withState } from 'recompose';
import moment from 'moment';
import { Link } from 'react-router-dom';
import { fetchGames } from '../actions/games';
import { getGames } from '../reducers';
import { Card, Row, Col } from 'react-bootstrap';
import Select from 'react-select';
import { statusesMap } from '../helpers/constants';
import { sortByStatus } from '../helpers/utils';

import '../styles/Home.scss';

const customStyles = {
  container: (provided) => ({
    ...provided,
    width: '16%',
    marginBottom: '2em',
  }),
  valueContainer: (provided) => ({
    ...provided,
    paddingLeft: 0,
  }),
  control: (provided) => ({
    ...provided,
    border: 0,
    borderBottom: '1px solid grey',
    borderRadius: 0,
    boxShadow: 'none',
  }),
  indicatorsContainer: (provided) => ({
    ...provided,
    display: 'none',
  }),
};

const customTheme = (theme) => ({
  ...theme,
  colors: {
    ...theme.colors,
    primary50: '#cccccc',
    primary25: '#cccccc',
    primary: '#808080',
  },
});

const Home = ({ gamesFiltered, options, setFilter }) => (
  <div className='Home'>
    <Select
      options={options}
      styles={customStyles}
      theme={customTheme}
      defaultValue={{ label: 'All', value: 'all' }}
      onChange={(el) => setFilter(el)}
    />
    <Row xs={1} md={3} className='g-4'>
      {gamesFiltered.map((game, i) => (
        <Link to={`/games/${game.fixture.id}`} key={i} className='game'>
          <Col>
            <Card
              className={`card ${statusesMap[
                game.fixture.status.short
              ].toLowerCase()}`}
            >
              <div className='imgContainer'>
                <Card.Img
                  variant='top'
                  src={game.teams.home.logo}
                  className='teamImg'
                />
                <Card.Img
                  variant='top'
                  src={game.teams.away.logo}
                  className='teamImg'
                />
              </div>
              <Card.ImgOverlay>
                <div
                  className={`status ${statusesMap[
                    game.fixture.status.short
                  ].toLowerCase()}`}
                >
                  {statusesMap[game.fixture.status.short]}
                </div>
              </Card.ImgOverlay>
              <Card.Body className='cardBody'>
                <Card.Title>
                  {game.teams.home.name} - {game.teams.away.name}
                </Card.Title>
                <Card.Text>
                  {moment(game.fixture.date).format('DD MMMM HH:mm YYYY')}
                </Card.Text>
              </Card.Body>
            </Card>
          </Col>
        </Link>
      ))}
    </Row>
  </div>
);

const enhancer = compose(
  withState('filter', 'setFilter', { label: 'All', value: 'all' }),
  withProps(() => ({
    options: [
      { label: 'All', value: 'all' },
      ...Object.entries(statusesMap).map((el) => ({
        label: el[1].charAt(0) + el[1].slice(1).toLowerCase(),
        value: el[0],
      })),
    ],
  })),
  connect(
    (state) => ({
      games: getGames(state),
    }),
    (dispatch) => ({
      fetchGames: () => dispatch(fetchGames()),
    })
  ),
  lifecycle({
    componentDidMount() {
      this.props.fetchGames();
    },
  }),
  withProps(({ games }) => ({
    gamesSorted: sortByStatus(games),
  })),
  withProps(({ gamesSorted, filter }) => ({
    gamesFiltered:
      filter.value === 'all'
        ? gamesSorted
        : gamesSorted.filter(
            (game) => game.fixture.status.short === filter.value
          ),
  }))
);

export default enhancer(Home);
