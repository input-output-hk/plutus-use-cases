import TokensList from './TokensList';

import '../styles/HomePage.scss';

const data = [
  { name: 'Cool token', price: '100' },
  { name: 'Golden kitten', price: '1000' },
  { name: 'Hero', price: '0.4' },
  { name: 'Mad head', price: '4' },
  { name: 'Naughty dog', price: '75' },
  { name: 'Gabriel', price: '999' },
];

const HomePage = () => (
  <div className='HomePage'>
    <h3 className='heading'>Storefront</h3>
    <TokensList tokens={data} />
  </div>
);

export default HomePage;
