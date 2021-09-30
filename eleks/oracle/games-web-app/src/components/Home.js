import { connect } from 'react-redux';
import { compose, lifecycle, withProps } from 'recompose';
import moment from 'moment';
import { Link } from 'react-router-dom';
import { fetchGames } from '../actions/games';
import { getGames } from '../reducers';
import { Card, Row, Col } from 'react-bootstrap';
import Select from 'react-select';

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

const Home = ({ games, options }) => (
  <div className='Home'>
    <Select
      options={options}
      styles={customStyles}
      theme={customTheme}
      defaultValue={{ label: 'All', value: 'all' }}
    />
    <Row xs={1} md={3} className='g-4'>
      {games.map((game, i) => (
        <Link to={`/games/${game.fixture.id}`} key={i} className='game'>
          <Col>
            <Card border='warning'>
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
                  className={`status ${game.fixture.status.short.toLowerCase()}`}
                >
                  {game.fixture.status.short}
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
  withProps(() => ({
    options: [
      { label: 'All', value: 'all' },
      { label: 'Closed', value: 'closed' },
      { label: 'In progress', value: 'live' },
      { label: 'Open', value: 'open' },
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
  })
);

export default enhancer(Home);
