import Coin from '../icons/coin.gif';
import Card from 'react-bootstrap/Card';
import { Link } from 'react-router-dom';

import '../styles/TokensList.scss';

const TokensList = ({ tokens }) => (
  <section className='TokensList'>
    {tokens &&
      tokens.map((token, i) => (
        <Link to={`/:${token.name}`} className='token-link' key={i}>
          <Card>
            <Card.Img
              className='image'
              variant='top'
              src={token.image ? `https://ipfs.io/ipfs/${token.image}` : Coin}
            />
            <Card.Body>
              <div className='name'>
                <Card.Subtitle className='mb-2 text-muted'>Name:</Card.Subtitle>
                <Card.Title>{token.name}</Card.Title>
              </div>
              {token.price && (
                <div className='price'>
                  <Card.Subtitle className='mb-2 text-muted'>
                    Price:
                  </Card.Subtitle>
                  <Card.Title>{token.price}$</Card.Title>
                </div>
              )}
            </Card.Body>
          </Card>
        </Link>
      ))}
  </section>
);

export default TokensList;
